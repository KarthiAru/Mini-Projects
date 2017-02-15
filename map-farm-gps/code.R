suppressMessages({
    library(geosphere)
    library(dplyr)
    library(data.table)
})
options(digits=11)

planter_harvester.join <- function(planter, harvester){
    
    cat("\n1. Reading Files...")
    planting_df <- readr::read_csv(planter)
    harvest_df <- readr::read_csv(harvester)
    
    b_long <- c(min(min(planting_df$long),min(harvest_df$long)),max(max(planting_df$long),max(harvest_df$long)))
    b_lat <- c(min(min(planting_df$lat),min(harvest_df$lat)),max(max(planting_df$lat),max(harvest_df$lat)))
    hc <- c((max(harvest_df$long)+min(harvest_df$long))/2, (max(harvest_df$lat)+min(harvest_df$lat))/2)
    pc <- c((max(planting_df$long)+min(planting_df$long))/2, (max(planting_df$lat)+min(planting_df$lat))/2)
    
    # vertical gridlines
    cat("\n2. Creating Gridlines...")
    x <- planting_df %>%
        filter(lat >= pc[2]-1e-05 & round(lat,5) <= pc[2]+1e-05) %>%
        mutate(long = round(long,5) + 5.4e-5) %>%
        arrange(long) %>% .$long %>% unique  
    x <- c(b_long[1]-5.4e-05, x, b_long[2]+5.4e-05)
    
    # horizontal gridlines
    d <- (b_lat[2] - b_lat[1]) / (length(x) - 2)
    y <- rep(d, (length(x) - 2))
    y <- round((b_lat[1] + cumsum(y)),6)
    y <- c(b_lat[1]-d, y, b_lat[2]+d)
    
    dfx <- tbl_df(x)
    dfx$value1 <- lead(dfx$value)-1e-07
    dfx <- dfx %>% na.omit()
    dfx$groupX <- 1:nrow(dfx)
    dfx <- data.table(dfx)
    
    dfy <- tbl_df(y)
    dfy$value1 <- lead(dfy$value)-1e-07
    dfy <- dfy %>% na.omit()
    dfy$groupY <- 1:nrow(dfy)
    dfy <- data.table(dfy)
    
    group <- expand.grid(groupX=c(1:nrow(dfx)),groupY=c(1:nrow(dfy)))
    group$value <- paste0(group$groupY,"-",group$groupX)
    group$group <- 1:nrow(group)
    group <- group %>% select(value, group)
    
    ########
    cat("\n3. Assigning Grids to Planter...")
    # create dummy lat-long ranges
    planting_df <- planting_df %>%
        mutate(long1 = round(long,5),
               long2 = round(long,5),
               lat1 = round(lat,6),
               lat2 = round(lat,6))
    planting_df <- data.table(planting_df)
    #setkey
    setkey(planting_df, long1, long2)
    setkey(dfx, value, value1)
    
    #vertical grids
    planting_grid <- foverlaps(planting_df, dfx, nomatch = NA) #left_join
    #setkey
    setkey(planting_grid, lat1, lat2)
    setkey(dfy, value, value1)
    
    #horizontal grids
    planting_grid <- foverlaps(planting_grid, dfy, nomatch = NA) #left_join
    planting_grid <- planting_grid %>%
        select_("long","lat","variety","seeding_rate","seed_spacing","speed","groupX","groupY") %>%
        mutate(value = paste0(groupY,"-",groupX)) %>% select(-groupX, -groupY) %>%
        left_join(group, by="value") %>% arrange(long,lat)
    
    #######
    cat("\n4. Assigning Grids to Harvester...")
    # create dummy lat-long ranges
    harvest_df <- harvest_df %>%
        mutate(long1 = round(long,5),
               long2 = round(long,5),
               lat1 = round(lat,6),
               lat2 = round(lat,6))
    harvest_df <- data.table(harvest_df)
    
    #setkey
    setkey(harvest_df, long1, long2)
    #left_join
    harvester_grid <- foverlaps(harvest_df, dfx, nomatch = 0) #left_join
    #setkey
    setkey(harvester_grid, lat1, lat2)
    #left_join
    harvester_grid <- foverlaps(harvester_grid, dfy, nomatch = 0) #left_join
    harvester_grid <- harvester_grid %>% 
        select_("long","lat","yield","groupX","groupY") %>%
        mutate(value = paste0(groupY,"-",groupX)) %>% select(-groupX, -groupY) %>%
        left_join(group, by="value") %>% arrange(long,lat)
    
    # create new variable to store data from planting
    harvester_grid <- harvester_grid %>%
        mutate(variety = NA,
               seeding_rate = NA,
               seed_spacing = NA,
               speed = NA,
               long1 = NA,
               lat1 = NA,
               dist = NA)
    
    ### Mapping planting to harvest
    cat("\n5. Joining Planter-Harvester...\n")
    timer <- Sys.time()
    pb <- txtProgressBar(max=nrow(harvester_grid), style = 3)
    for(i in 1:nrow(harvester_grid)){
        harvest.gps <- harvester_grid[i,]       
        n <- unlist(strsplit(harvest.gps$value, '-'))
        row <- as.integer(n[1])
        col <- as.integer(n[2])        
        # filter the rows from planting - nearest neighbour search      
        neighbour <- c(paste0(row,"-",col-1),
                           paste0(row-1,"-",col),
                           paste0(row,"-",col),
                           paste0(row+1,"-",col),
                           paste0(row,"-",col+1))         
        planter.gps <- planting_grid %>% filter(value %in% neighbour)
        
        if(nrow(planter.gps)>0){
            # store geo-coordinates for vector based operation
            planter.gps$long1 <- harvest.gps$long
            planter.gps$lat1 <- harvest.gps$lat
            
            # find the distance between geo-coordinates
            planter.gps$dist <- distm(as.matrix(planter.gps[,c('long','lat')]), as.matrix(planter.gps[,c('long1','lat1')]), fun = distHaversine)[,1]
            planter.gps <- planter.gps %>% filter(dist == min(dist)) %>% select(-long1, -lat1, -group)
            
            # map the data back to harvest
            harvester_grid[i, "variety"] <- planter.gps[1,"variety"]
            harvester_grid[i, "seeding_rate"] <- planter.gps[1,"seeding_rate"]
            harvester_grid[i, "seed_spacing"] <- planter.gps[1,"seed_spacing"]
            harvester_grid[i, "speed"] <- planter.gps[1,"speed"]
            harvester_grid[i, "long1"] <- planter.gps[1,"long"]
            harvester_grid[i, "lat1"] <- planter.gps[1,"lat"]
            harvester_grid[i, "dist"] <- planter.gps[1,"dist"]
        }
        setTxtProgressBar(pb, i)
    }
    cat("\n***Process completed in",round(difftime(Sys.time(), timer, tz = "", units = "mins"),2),"min!***")
    
    # Save file
    cat("\n6. Saving file...")
    write.csv(harvester_grid %>% select(long, lat, yield, variety, seeding_rate, seed_spacing, speed),
                     file = "planter-harvester.csv", row.names=FALSE)
}