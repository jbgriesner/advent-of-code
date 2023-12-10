use std::{
    collections::{BTreeMap, HashSet},
    fmt,
};

type NodeIndex = usize;
type EdgeIndex = usize;

#[derive(Clone)]
struct Bfs<'graph> {
    graph: &'graph Graph,
}

impl<'graph> Bfs<'graph> {
    fn run(&mut self) -> Vec<(NodeIndex, u32)> {
        let start_idx = self.graph.start_index;
        let mut visited: HashSet<NodeIndex> = HashSet::new();

        let mut parents_map: BTreeMap<NodeIndex, (Option<NodeIndex>, u32)> = BTreeMap::new();
        parents_map.insert(start_idx, (None, 0));
        visited.insert(start_idx);

        let ultimate = &mut 0;
        let distance = &mut 0;

        self.bfs(
            start_idx,
            &mut parents_map,
            start_idx,
            &mut visited,
            ultimate,
            distance,
        );

        println!("ULTIMATE: {}", ultimate);

        let mut rez = vec![(*ultimate, 1)];
        // println!("parents_map: {:?}", parents_map);

        let curr = &mut *ultimate;
        while let Some((node, d)) = parents_map.get(curr) {
            // println!("NOOOOO");
            match (node, d) {
                (None, _) => break,
                (Some(n), d) => {
                    *curr = *n;
                    rez.push((*n, *d))
                }
            }
        }
        rez
    }

    fn bfs(
        &mut self,
        current_idx: NodeIndex,
        map: &mut BTreeMap<NodeIndex, (Option<NodeIndex>, u32)>,
        last: NodeIndex,
        visited: &mut HashSet<NodeIndex>,
        ultimate: &mut NodeIndex,
        distance: &mut u32,
    ) {
        // println!("Visited node: {}", current_idx);
        let neighbors: Vec<NodeIndex> = self.graph.successors(current_idx).collect();
        // println!("\tNeighbors: {:?}", neighbors);

        for neighbor in neighbors {
            *distance = *distance + 1;
            let n = self.graph.get_node_at_idx(Some(neighbor)).unwrap();
            if n.data == 'S' && last != self.graph.start_index {
                // println!("\tReached END!!");
                // map.insert(neighbor, Some(current_idx));
                *ultimate = current_idx;
            } else {
                if !visited.contains(&neighbor) {
                    visited.insert(neighbor);
                    map.insert(neighbor, (Some(current_idx), distance.clone()));
                    self.bfs(neighbor, map, current_idx, visited, ultimate, distance);
                }
            }
        }
    }
}

pub struct Successors<'graph> {
    graph: &'graph Graph,
    current_edge_index: Option<EdgeIndex>,
}

impl<'graph> Iterator for Successors<'graph> {
    type Item = NodeIndex;

    fn next(&mut self) -> Option<NodeIndex> {
        match self.current_edge_index {
            None => None,
            Some(edge_num) => {
                let edge = &self.graph.edges[edge_num];
                self.current_edge_index = edge.next_outgoing_edge;
                Some(edge.target)
            }
        }
    }
}

#[derive(Debug)]
struct EdgeData {
    target: NodeIndex,
    next_outgoing_edge: Option<EdgeIndex>,
}

#[derive(Debug, Clone)]
struct NodeData {
    data: char,
    first_outgoing_edge: Option<EdgeIndex>,
}

impl NodeData {
    fn check_north(&self, other_node: Self) -> bool {
        match self.data {
            'S' | '|' | 'L' | 'J' => match other_node.data {
                '|' | '7' | 'F' | 'S' => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn check_south(&self, other_node: Self) -> bool {
        // print!("self: {}, other: {}", self.data, other_node.data);
        match self.data {
            'S' | '|' | '7' | 'F' => match other_node.data {
                '|' | 'L' | 'J' | 'S' => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn check_east(&self, other_node: Self) -> bool {
        match self.data {
            'S' | '-' | 'L' | 'F' => match other_node.data {
                '-' | 'J' | '7' | 'S' => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn check_west(&self, other_node: Self) -> bool {
        match self.data {
            'S' | '-' | 'J' | '7' => match other_node.data {
                '-' | 'L' | 'F' | 'S' => true,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug)]
struct Graph {
    poz_to_index: BTreeMap<(usize, usize), NodeIndex>,
    index_to_poz: BTreeMap<NodeIndex, (usize, usize)>,
    nodes: Vec<NodeData>,
    edges: Vec<EdgeData>,
    start_index: NodeIndex,
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut rez = "Graph = \n\t\t".to_string();
        for n in 0..self.nodes.len() {
            if n == self.start_index {
                rez += &format!(
                    "\n\t\t- START-{}: {:?} :\t\t",
                    n,
                    self.get_poz_from_index(n)
                );
            } else {
                rez += &format!("\n\t\t- node-{}: {:?} :\t\t", n, self.get_poz_from_index(n));
            }

            let mut init = false;
            for i in self.successors(n) {
                if !init {
                    rez += "\n\t\t\t\t";
                    init = true;
                }
                rez += &format!(" idx: {} -> {:?}\n\t\t\t\t", i, self.get_poz_from_index(i));
            }
        }

        write!(f, "{}", rez)
    }
}

impl Graph {
    fn check_if_inside(&self, cycle: &[(usize, usize)], x: usize, y: usize) -> bool {
        if cycle.contains(&(x, y)) {
            return false;
        }

        let mut count: u32 = 0;

        // let min = std::cmp::min(max_x_cycle, x);

        let max_x_at_this_y = cycle
            .iter()
            .filter(|(_, yy)| *yy == y)
            .map(|(xx, _)| *xx)
            .max()
            .unwrap();

        if x > max_x_at_this_y {
            return false;
        }

        let min = std::cmp::min(x, max_x_at_this_y);

        for candidate_x in 0..(min) {
            if cycle.contains(&(candidate_x, y)) {
                count += 1;
            }
        }
        if count == 0 {
            false
        } else if count % 2 == 0 {
            false
        } else {
            true
        }
    }

    fn compute_area_loop(&self, cycle: Vec<NodeIndex>) -> usize {
        let cycle: Vec<(usize, usize)> =
            cycle.iter().map(|v| self.get_poz_from_index(*v)).collect();

        let mut c = cycle.clone();
        c.sort();
        println!("cycle: {:?}", c);

        let width = self.width();
        let height = self.height();

        let mut is_inside = vec![vec![false; width]; height];

        for x in 0..width {
            for y in 0..height {
                // println!("compute_area_loop: x, y {}, {}", x, y);
                if self.check_if_inside(&cycle, x, y) {
                    is_inside[y][x] = true;
                }
            }
        }

        let i = &mut 0;
        for r in is_inside.clone() {
            println!("row {}: {:?}", i, r);
            *i += 1;
        }

        is_inside
            .iter()
            .flat_map(|row| row.iter())
            .filter(|&&x| x)
            .count()
    }

    fn get_index_node_at(&self, x: i32, y: i32) -> Option<NodeIndex> {
        if x < 0 || y < 0 {
            None
        } else {
            self.poz_to_index
                .get(&(x as usize, y as usize))
                .map(|v| v.clone())
        }
    }

    fn get_poz_from_index(&self, idx: NodeIndex) -> (usize, usize) {
        self.index_to_poz.get(&idx).unwrap().clone()
    }

    fn bfs(&self) -> Bfs {
        Bfs { graph: self }
    }

    fn successors(&self, source: NodeIndex) -> Successors {
        let first_outgoing_edge = self.nodes[source].first_outgoing_edge;
        Successors {
            graph: self,
            current_edge_index: first_outgoing_edge,
        }
    }

    fn get_node_at_idx(&self, idx: Option<NodeIndex>) -> Option<NodeData> {
        match idx {
            None => None,
            Some(idx) => self.nodes.get(idx).map(|n| n.clone()),
        }
    }

    fn width(&self) -> usize {
        self.poz_to_index
            .keys()
            .map(|(x, _)| x)
            .cloned()
            .max()
            .unwrap()
            + 1
    }

    fn height(&self) -> usize {
        self.poz_to_index
            .keys()
            .map(|(_, y)| y)
            .cloned()
            .max()
            .unwrap()
            + 1
    }

    fn init_edges(&mut self) {
        let width = self.width();
        let height = self.height();

        for x in 0..width {
            for y in 0..height {
                // println!("x: {}, y: {}", x, y);
                let current_idx = self
                    .get_index_node_at(x as i32, y as i32)
                    .map(|v| v.clone());
                // println!("current_idx: {:?}", current_idx);
                let current_node = self.get_node_at_idx(current_idx).unwrap();

                if current_node.data != '.' {
                    let north_idx = self.get_index_node_at(x as i32, y as i32 - 1);
                    if let Some(node) = self.get_node_at_idx(north_idx) {
                        if current_node.check_north(node) {
                            self.add_edge(current_idx.unwrap(), north_idx.unwrap())
                        }
                    }

                    let south_idx = self.get_index_node_at(x as i32, (y + 1) as i32);
                    if let Some(node) = self.get_node_at_idx(south_idx) {
                        if current_node.check_south(node) {
                            self.add_edge(current_idx.unwrap(), south_idx.unwrap())
                        }
                    }

                    let east_idx = self.get_index_node_at((x + 1) as i32, y as i32);
                    if let Some(node) = self.get_node_at_idx(east_idx) {
                        if current_node.check_east(node) {
                            self.add_edge(current_idx.unwrap(), east_idx.unwrap())
                        }
                    }

                    let west_idx = self.get_index_node_at(x as i32 - 1, y as i32);
                    if let Some(node) = self.get_node_at_idx(west_idx) {
                        if current_node.check_west(node) {
                            self.add_edge(current_idx.unwrap(), west_idx.unwrap())
                        }
                    }
                }
            }
        }
    }

    fn new() -> Self {
        Graph {
            poz_to_index: BTreeMap::new(),
            index_to_poz: BTreeMap::new(),
            nodes: vec![],
            edges: vec![],
            start_index: 0,
        }
    }

    fn add_node(&mut self, data: char, x: usize, y: usize) {
        let index = self.nodes.len();
        self.nodes.push(NodeData {
            data,
            first_outgoing_edge: None,
        });
        self.poz_to_index.insert((x, y), index);
        self.index_to_poz.insert(index, (x, y));
        if data == 'S' {
            self.start_index = index;
        }
    }

    fn add_edge(&mut self, source: NodeIndex, target: NodeIndex) {
        let node_data = &mut self.nodes[source];

        let edge_index = self.edges.len();

        self.edges.push(EdgeData {
            target: target,
            next_outgoing_edge: node_data.first_outgoing_edge,
        });

        node_data.first_outgoing_edge = Some(edge_index);
    }
}

fn parse_lines(input: &str) -> Vec<Vec<char>> {
    input
        .lines()
        .map(|l| l.chars().collect::<Vec<char>>())
        .collect()
}

fn init_graph(lines: Vec<Vec<char>>) -> Graph {
    let x = &mut 0;
    let y = &mut 0;

    let mut g = lines.iter().fold(Graph::new(), |graph, line| {
        *x = 0;

        let g = line.iter().fold(graph, |mut g, c| {
            // println!("x: {}, y: {}, char: {}", x, y, *c);
            g.add_node(*c, *x, *y);
            *x += 1;
            g
        });
        *y += 1;
        g
    });

    g.init_edges();
    g
}

pub fn process(s: &str) -> String {
    let lines = parse_lines(s);
    let graph = init_graph(lines);

    // println!("graph: {:?}", graph);
    println!("graph: {}", graph);

    let mut bfs = graph.bfs();
    let r = bfs
        .run()
        .iter()
        .map(|(v1, _)| v1)
        .cloned()
        .collect::<Vec<usize>>();
    println!("run: {:?}", r);

    graph.compute_area_loop(r.clone()).to_string()
}
