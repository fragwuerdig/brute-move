# Brute Move!

This is yet another chess engine in Rust. This one is optimized for disk space footprint and minimal dependencies. Moves are being generated and validated during runtime and with a brute-force algo (hence "Brute Move!"). Other chess engines use pre-calculated magic lookup tables to be able to generate moves at lightning speed. These tables can make up to 75% of the resulting binary storage footprint!

### Purpose

I wrote this lib so I could use it in a browser game that I had in mind (and which would make use of the WASM runtime). I wanted to make sure it is as tiny as possible for fast loading times on the web. After I wrote this lib, I freakin' stumbled across [cosmwasm-chess](https://github.com/jeremyfee/cosmwasm-chess), which kind of fits my needs already :D

There are some downsides to that library though... one of which being default pawn-queen promotion. Anyway, I could have just forked Jeremy's work... Well... you know how that works: you stick to what you're familiar with.

### Pros:

- Minimal dependencies  
- Low binary size footprint  
- Naive but clear implementation path  

### Cons:

- Unsuitable for AI move generation  
- Unsuitable for automated game analysis  

## Wen Release?

At some time in the future I might decide this thing is stable enough to be uploaded to https://crates.io – for now you can just download it and locally bind it into your own Rust project. Have fun!
