require(tidyverse)
require(tidygraph)
require(ggraph)

# Example tibble from Twitter

tst <- tribble(
  ~node, ~cover,
  "a", "b",
  "c", "b",
  "b", "d",
  "e", "f",
  "f", "g"
)

# Original example of desired output

tst %>% mutate(
  root = c(rep("d", 3), "g", "g")
)


# Creates a list of nodes with id numbers for each

gen_node_list <- function(x) {
  nodes <- full_join((x %>% 
                        distinct(node)),
                     (x %>%
                        distinct(cover) %>%
                        rename(node = cover)))
  return(nodes %>% rowid_to_column("id"))
}

# Converts original tibble in to one where id numbers are used
# in format using "from" and "to" columns

gen_edge_list <- function(x, nodes = gen_node_list(x)) {
  edges <- x %>% 
    left_join(nodes, by = "node") %>%
    left_join(nodes, by = c("cover" = "node")) %>%
    rename(from = id.x,
           to = id.y)
  return(edges %>% select(from, to))
}

# Calls functions for creating node and edge lists
# Before generating a directed network using tidygraph::tbl_graph

gen_network <- function(x) {
  nodes <- gen_node_list(x)
  edges <- gen_edge_list(x, nodes)
  net <- tbl_graph(nodes, edges, directed = TRUE)
  return(list(nodes = nodes,
              edges = edges,
              net = net))
}


# Generate list of children of any given node
# TODO: First entry of results is root node, need to fix

get_children <- function(x, p) {
  child_list <- neighbors(x, p, mode = "in")
  result <- c(p)
  if (length(child_list) == 0) {
    return(result)
  } else {
    for (child in child_list) {
      result <- c(result, get_children(x, child))
    }
  }
  return(result)
}


# Using directed network, determine nodes which are roots
# Then create list of children of each root node

get_roots <- function(x) {
  roots <- which(sapply(sapply(V(x), function(y) neighbors(x, y, mode = "out")), length) == 0)
  results <- NULL
  for (r in roots) {
    z <- get_children(x, r)
    results <- bind_rows(results,
                         tibble(root = rep(r, (length(z) - 1)),
                                node = z[2:length(z)]))
  }
  return(results)
}

# Call all of the things required and returns result in form of tibble with edge list
# But with the "root" column being the root node for each node in the "from" column
# TODO: Return results in original format using node names rather than ids?

do_it_all <- function(x) {
  overall_net <- gen_network(x)
  root_nodes <- get_roots(overall_net$net)
  return(overall_net$edges %>%
           left_join(root_nodes, by = c("from" = "node")))
}

do_it_all(tst)