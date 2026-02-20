[![R](https://img.shields.io/badge/R-%23276DC3.svg?logo=r&logoColor=white)](#)

# VAR — Visualising and Analysing Results <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Shiny_hex_logo.svg/1200px-Shiny_hex_logo.svg.png" align="right" width="100" alt="Shiny logo" />
A Shiny app for exploring and analysing sports betting data that I collected. “Killing two birds with one stone”, this app gave me the chance to work on something that contributes to my final‑year project while also being eligible for submission to the Maynooth DSS 2025/26 Shiny competition.

Live app: https://03zmve-owen0f0o0connor.shinyapps.io/Var-/

- Interactive exploration of sports betting data: odds, results, and other metrics.
- A place to try to identify patterns/market inefficiencies that can inform the building of models/market strategies.
- Rapidly sharing ideas — presented in an intuitive way, with help pages for each plot and a video walk‑through for greater accessibility.

## Inspiration
[Phil Maguire](https://www.cs.nuim.ie/~pmaguire/) is in no small part responsible for me getting the opportunity to study data science in Maynooth, so when I got the chance to take on a final‑year project with him as my supervisor it felt like a full‑circle story.

The project was described in a way that also piqued my interest — mentions of collecting data and comparing trends to a random walk.  
An opportunity to apply statistics somewhere with real‑world applications!

On the Shiny side, I wanted to push beyond what I learned last year: from a single app.R file to a structured directory with clear separation of concerns. I introduced modules from [Mastering Shiny](https://mastering-shiny.org/scaling-modules.html) and integrated techniques appropriate for handling larger datasets. The original version of this app used an external database with {dbplyr}, meaning it could grow alongside my database and handle greater volumes of data by leveraging server‑side SQL queries.

## Building on recent work: BETtR <img src="https://raw.githubusercontent.com/ThatObiGuy/BETtR/main/man/figures/logo.png" align="right" width="100" alt="BETtR logo" />
This app builds on work started while developing a package with friends: **BETtR** — a project clearly applicable to the same theme. One of the first plots you’ll see within the VAR dashboard is largely adapted from BETtR’s generic `plot()` function.  
Working on multiple projects around the same concept has given me more exposure to the ideas and let me keep researching while also progressing other goals (like keeping up with assignments!).

Take a closer look: https://github.com/ThatObiGuy/BETtR

Beyond the plotting function, work on BETtR provided a helpful foundation for thinking about how to structure the data I was collecting, and experience manipulating it.

## Future improvements and ideas
The original app was built in a scalable fashion to grow with the database. Due to deployment difficulties, this version relies on a local subset of the data.  
That’s valid for its purposes, but if I’d known this would be the case in advance I’d store only the data required for plotting in an uncompressed `.RData` file — likely speeding things up. I’d also be interested in measuring that difference.

Separately, in the version that works with a database connection, I’d like to add another tab where I formally test some hypotheses and simulate the performance of different trading strategies informed by insights from the plots.

### <img src="https://asset.brandfetch.io/idAnDTFapY/idDdbxxs3M.png"  width="20" alt="Miro logo" /> Note on learning and process 
I’ve been documenting some of the thinking and learning I’ve been doing while developing this app — and the larger project — on a Miro board. You can browse it below (embed) or open it directly.

- Direct link: https://miro.com/app/board/uXjVJ_qFffo=/?share_link_id=291096266274

## GenAI usage
To aid development I used a [Perplexity](https://www.perplexity.ai/) space loaded with a number of sites I thought could be helpful for best practice:

- https://shiny.posit.co
- https://shiny.posit.co/r/articles/improve/modules
- https://mastering-shiny.org
- https://adv-r.hadley.nz
- https://dbplyr.tidyverse.org
- https://engineering-shiny.org
- https://r-pkgs.org
- https://rstudio.github.io/DT/shiny.html

Developing this in parallel with other commitments, I found it helpful to be able to “chat with an expert” on relevant topics: best‑practice approaches to unifying legends across modules, appropriate packages for displaying interactive tables, and getting to grips with {dbplyr}. When I didn’t have time to read the books and documentation, the tool could point me to the right page, paragraph, and sentence.

---

## <img src="https://cdn.mulife.ie/user_files/logo/10306/c9cfa9bc1513831fd65d905abc473352.jpg" width="30" alt="DSS logo" /> Data Science Society 

If you’re somehow not already aware, please check out the host of this great competition: the [Maynooth Data Science Society](https://mulife.ie/society/data-science).


## Thank you(s)
Thanks for checking this out!  
Thanks to the wonderful Gwenaelle Mathieu for drawing out the help graphics for me.  
Thanks also to all the brilliant people that gave feedback throughout the development process. (even if some of it has yet to be implemented...)  
If you’re interested, I intend to develop this app further outside of the main branch, which will remain as is until the end of the competition.