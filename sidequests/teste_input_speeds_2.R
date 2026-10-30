
mini_feed_3 <- merge_gtfs(feeds, prefix = c("y12", "y15")) |>
	filter_by_trip_id(c("y12_408A-10-0", "y15_408A-10-0", "y12_1016-10-0", "y15_1016-10-0"))

mini_feed_4 <- gtfstools::fix_gtfs(mini_feed_3)
mini_feed_4 <- gtfstools::combine_nearby_stops(mini_feed_4, 4)



speeds <- gtfstools::get_trip_speed(mini_feed_4, by="segment")

speeds2 <- speeds |> 
    group_by(from_stop_id, to_stop_id) |> 
  summarise(avg_speed = mean(speed))

# filtrar 2012
mini_feed_4_12 <- mini_feed_4
bmini_feed_5 <- gtfstools::set_trip_speed(mini_feed_4_12, route_type=3,
  speed = speeds2$avg_speed,
  first_stop = speeds2$from_stop_id,
  last_stop = speeds2$to_stop_id
)


g <- gtfstools::build_graph(gtfs = mini_feed_4, 
  from = "01:00:00", to = "18:00:00",
    format = 'gmns'
  )


g

stop_nodes <- g$node |> 
  filter(node_type =="stop") |> 
  pull(node_id)


s <- g$link |> 
  filter( 
    from_node_id %in% stop_nodes &
    to_node_id %in% stop_nodes
) |> 
  group_by(from_node_id, to_node_id) |> 
  summarise(avg_speed = mean(travel_time))

head(s)

data.table::setDT(s)
s[g$node, on = c('from_node_id'='node_id'), from_stop_id  := i.stop_id]
s[g$node, on = c('to_node_id'='node_id'), to_stop_id  := i.stop_id]

head(s)
