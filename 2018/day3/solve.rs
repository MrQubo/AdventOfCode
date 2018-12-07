use std::fs;

#[macro_use] extern crate text_io;

// mod intervaltree;
// use intervaltree::{ Tree, Mergeable, Concatenable };


#[derive(Clone, Copy, Debug, Default)]
struct Claim
{
   id: i32,
   x: usize,
   y: usize,
   width: usize,
   height: usize,
}

const GRID_SIZE: usize = 1000;


fn parse(input : String) -> Vec<Claim>
{
   let mut vec: Vec<Claim> = Vec::new();

   for line in input.split('\n') {
      if line.is_empty() {
         continue;
      }

      let id: i32;
      let x: usize;
      let y: usize;
      let width: usize;
      let height: usize;

      scan!(line.bytes() => "#{} @ {},{}: {}x{}", id, x, y, width, height);

      vec.push(Claim { id, x, y, width, height });
   }

   vec
}


fn get_grid<I>(claims: I) -> (Vec<Vec<Vec<i32>>>, usize)
   where
      I: Iterator<Item = Claim>,
{
   let mut claim_count: usize = 0;
   let mut grid: Vec<Vec<Vec<i32>>> = Vec::with_capacity(GRID_SIZE);

   for _ in 0..GRID_SIZE {
      let mut row = Vec::with_capacity(GRID_SIZE);
      for _ in 0..GRID_SIZE {
         row.push(Vec::new());
      }
      grid.push(row);
   }

   for claim in claims.into_iter() {
      claim_count += 1;
      for dy in 0..claim.height {
         for dx in 0..claim.width {
            grid[claim.y + dy][claim.x + dx].push(claim.id);
         }
      }
   }

   (grid, claim_count)
}


fn solve(input: String)
{
   let claims = parse(input);

   let (grid, claim_count) = get_grid(claims.into_iter());
   let mut count: usize = 0;
   let mut overlapping: Vec<bool> = Vec::with_capacity(claim_count);
   overlapping.resize(claim_count, false);

   for y in 0..GRID_SIZE {
      for x in 0..GRID_SIZE {
         if grid[y][x].len() >= 2 {
            count += 1;
            for claim_id in grid[y][x].iter() {
               overlapping[(claim_id - 1) as usize] = true;
            }
         }
      }
   }

   let perfect_id = overlapping.into_iter().position(|x| !x).unwrap() + 1;

   println!("part1: {}\npart2: {}", count, perfect_id);
}


fn main()
{
   let input = fs::read_to_string("input")
      .expect("Something went wrong reading the file");

   solve(input);
}
