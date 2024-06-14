openAlex4NodeXL <- function(keywords, pub_start_date, pub_end_date) {
  
  keywords <- keywords
  pub_start_date <- pub_start_date
  pub_end_date <- pub_end_date

  # create search engine function
  search_engine <- function(keywords, pub_start_date, pub_end_date) {
    # load software libraries
    suppressPackageStartupMessages(library(openalexR))
    suppressPackageStartupMessages(library(tidyverse))

    # set options
    options(openalexR.mailto = "youremail@email.com") # replace with your email address

    # search engine
    works_search <- oa_fetch(
      entity = "works",
      title.search = c(keywords),
      cited_by_count = ">50",
      from_publication_date = pub_start_date,
      to_publication_date = pub_end_date,
      options = list(sort = "cited_by_count:desc"),
      verbose = FALSE
    )

    return(works_search)
  }
  
  # fetch data from openalex.org api 
  search_data <- search_engine(keywords, pub_start_date, pub_end_date)

  # grab authors and group them according to collaboration
  authors_collaboration_groups <- list()
  for (i in 1:nrow(search_data)) {
    authors_collaboration_groups[[i]] <- search_data$author[[i]][2]
  }

  all_authors <- c()
  for (i in 1:length(authors_collaboration_groups)) {
    all_authors <- c(all_authors, authors_collaboration_groups[[i]][[1]])
  }

  # grab author position
  authors_position <- list()
  for (i in 1:nrow(search_data)) {
    authors_position[[i]] <- search_data$author[[i]][4]
  }

  all_authors_positions <- c() # grab all authors positions
  for (i in 1:length(authors_position)) {
    all_authors_positions <- c(all_authors_positions, authors_position[[i]][[1]])
  }

  # grab author affiliation
  authors_affiliation <- list()
  for (i in 1:nrow(search_data)) {
    authors_affiliation[[i]] <- search_data$author[[i]][7]
  }

  all_authors_affiliation <- c() # grab all authors affiliations
  for (i in 1:length(authors_affiliation)) {
    all_authors_affiliation <- c(all_authors_affiliation, authors_affiliation[[i]][[1]])
  }

  # grab authors institution country code
  authors_institution_country_code <- list()
  for (i in 1:nrow(search_data)) {
    authors_institution_country_code[[i]] <- search_data$author[[i]][9]
  }


  all_authors_institution_country_code <- c() # grab all authors institution country code
  for (i in 1:length(authors_institution_country_code)) {
    all_authors_institution_country_code <- c(all_authors_institution_country_code, authors_institution_country_code[[i]][[1]])
  }

  # grab author institution type
  authors_institution_type <- list()
  for (i in 1:nrow(search_data)) {
    authors_institution_type[[i]] <- search_data$author[[i]][10]
  }


  all_authors_institution_type <- c() # grab all authors institution type
  for (i in 1:length(authors_institution_type)) {
    all_authors_institution_type <- c(all_authors_institution_type, authors_institution_type[[i]][[1]])
  }

  # get length of each authors collaboration
  authors_length <- c()
  for (authors in 1:length(authors_collaboration_groups)) {
    authors_length <- c(authors_length, authors_collaboration_groups[[authors]] |> nrow())
  }


  # create authors data frame
  authorAtt_df <- data.frame(
    Authors = all_authors,
    Position = all_authors_positions,
    Affiliation = all_authors_affiliation,
    Institution = all_authors_institution_type
  )

  # I did not want to have to use underscore to separate
  # the two words (Institution_Country). That is why I
  # created that column in the data frame using back ticks
  # instead as shown below
  authorAtt_df$`Institution Country` <- all_authors_institution_country_code

  # publication attributes
  # grab all publications

  publications <- list()
  for (i in 1:nrow(search_data)) {
    publications[[i]] <- rep(search_data$display_name[i], each = authors_length[i])
  }

  all_publications <- c()
  for (i in 1:length(publications)) {
    all_publications <- c(all_publications, publications[[i]])
  }

  # grab all so
  pub_so <- list()
  for (i in 1:nrow(search_data)) {
    pub_so[[i]] <- rep(search_data$so[i], each = authors_length[i])
  }

  all_so <- c()
  for (i in 1:length(pub_so)) {
    all_so <- c(all_so, pub_so[[i]])
  }

  # grab all host organization
  hostOrg <- list()
  for (i in 1:nrow(search_data)) {
    hostOrg[[i]] <- rep(search_data$host_organization[i], each = authors_length[i])
  }

  all_hostOrg <- c()
  for (i in 1:length(hostOrg)) {
    all_hostOrg <- c(all_hostOrg, hostOrg[[i]])
  }

  # grab all cited by count
  citedby_count <- list()
  for (i in 1:nrow(search_data)) {
    citedby_count[[i]] <- rep(search_data$cited_by_count[i], each = authors_length[i])
  }

  all_citedby_count <- c()
  for (i in 1:length(citedby_count)) {
    all_citedby_count <- c(all_citedby_count, citedby_count[[i]])
  }

  # grab all publication year
  pub_year <- list()
  for (i in 1:nrow(search_data)) {
    pub_year[[i]] <- rep(search_data$publication_year[i], each = authors_length[i])
  }

  all_pub_year <- c()
  for (i in 1:length(citedby_count)) {
    all_pub_year <- c(all_pub_year, pub_year[[i]])
  }

  # grab all type
  type <- list()
  for (i in 1:nrow(search_data)) {
    type[[i]] <- rep(search_data$type[i], each = authors_length[i])
  }

  all_type <- c()
  for (i in 1:length(type)) {
    all_type <- c(all_type, type[[i]])
  }

  # grab all abstract
  abstract <- list()
  for (i in 1:nrow(search_data)) {
    abstract[[i]] <- rep(search_data$ab[i], each = authors_length[i])
  }

  all_abstracts <- c()
  for (i in 1:length(abstract)) {
    all_abstracts <- c(all_abstracts, abstract[[i]])
  }

  # update the authors data frame
  {
    authorAtt_df$Publication <- all_publications
    authorAtt_df$`Abstract` <- all_abstracts
    authorAtt_df$`Publication Type` <- all_type
    authorAtt_df$`Publication Year` <- all_pub_year
    authorAtt_df$`Cited By Count` <- all_citedby_count
    authorAtt_df$`Host Organization` <- all_hostOrg
    authorAtt_df$SO <- all_so
  }


  # filter out missing values from the data frame
  authorAtt_df <- authorAtt_df |>
    na.omit()

  # move abstract column to behind Publication
  authorAtt_df <- authorAtt_df |>
    relocate(Abstract, .after = Publication)

  # rearrange columns for NodeXL flat file csv format
  authorAtt_df <- authorAtt_df |>
    relocate(Publication, .after = Authors)


  # rename columns
  colnames(authorAtt_df)[c(1:12)] <- c(
    "Vertex1",
    "Vertex2",
    "Vertex1 Position",
    "Vertex1 Affiliation",
    "Vertex1 Institution",
    "Vertex1 Institution Country",
    "Vertex2 Abstract",
    "Vertex2 Type",
    "Vertex2 Publication Year",
    "Vertex2 Cited By Count",
    "Vertex2 Host Organization",
    "Vertex2 SO"
  )



  return(authorAtt_df)
  

  
}

# # test software program
# mydata <- openAlex4NodeXL(
#   keywords = c("software", "information"),
#   pub_start_date = "2019-01-01",
#   pub_end_date = "2023-09-30"
# )

# # view returned data
# mydata |> view()
