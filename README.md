# cRimson
Tools for social media analysis (together with Crimson Hexagon)

## Installation
    install.packages("devtools")
    library(devtools)
    devtools::install_github("ccjolley/cRimson")
    library(cRimson)

## Examples

I've included a sample dataset called `sample_tweets`; you can convert it to a graph using:

    library(dplyr)
    g <- ws_to_graph(sample_tweets) %>%
      graph_lcc()

To get a simple visualization of the network that highlights major influencers (using the PageRank algorithm):

    sma_plot(g)
    
![sma_plot1](/img/sma_plot1.png)
    
If you want to see exactly who those major influencers are, you can get a color-coded bar chart:

    sma_bar(g)
    
![sma_bar1](/img/sma_bar1.png)

If you need to use a metric other than PageRank, you can specify it using the `highlight` argument:

    library(igraph)
    b <- betweenness(g)
    sma_plot(g,highlight=b)
    sma_bar(g,highlight=b)
    
![sma_plot2](/img/sma_plot2.png)

![sma_bar2](/img/sma_bar2.png)

The cRimson package also contains tools for the analysis and visualization of communities, building upon the wide variety of community detection algorithms offered by `igraph`. To visualize a community breakdown:

    fg <- g %>%
        as.undirected() %>%
        simplify() %>%
        fastgreedy.community()
    community_plot(g,fg)
    
![sma_plot3](/img/sma_plot3.png)

One way to capture what is happening in these communities is to find the "mayor" of each community -- the node that ranks the highest according to some graph metric (PageRank by default). To see exactly where these mayors are located in the network, use the `extra` argument in `sma_plot()` and `sma_bar()`. Note that color-coding is the same as in the plot above.

    m <- mayors(g,fg)
    sma_plot(g,extra=m)
    sma_bar(g,extra=m)
  
![sma_plot4](/img/sma_plot4.png)
![sma_bar4](/img/sma_bar4.png)

Comparing this plot to the community plot above will give you a sense of how the communities "led" by each of these handles extend beyond those with whom they have direct interactions.
