# Clean up synonymous species names 
# full_on_cover <- get_longform_cover(neon_div_object, scale = "plot", fix_unks = fix_unks)

all_sp=sort(unique(full_on_cover$scientificName))

species_names=tolower(all_sp)


species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\(l.\\).+$",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+-\\w+) \\(l.\\).+$",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "^(\\w+ \\w+) \\(.+\\).*$",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+-\\w+) l.$",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) l\\..*$",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\w+ ssp.*",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\w+ var.*",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\w+\\. ssp.*",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\w+\\. var.*",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\w+$",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) \\w+\\.$",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+) .*",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ \\w+-\\w+) .*",replacement = "\\1")

species_names=gsub(x = species_names,pattern = "(^\\w+ ×\\w+-\\w+).*$",replacement = "\\1")
species_names=gsub(x = species_names,pattern = "(^\\w+ ×\\w+).*$",replacement = "\\1")


species_names=gsub(x = species_names,pattern = "(^\\w+ )\\w+/\\w+$",replacement = "\\1sp.")
species_names=gsub(x = species_names,pattern = "^\\w+ \\w+/.+$",replacement = "unknown plant")



######
all_sp[grep(x = all_sp,pattern = "^\\w+ \\w+ \\w+\\.$")]

all_sp[grep(x = species_names,pattern = "(^\\w+ \\w+) \\w+\\.$")]
species_names[grep(x = species_names,pattern = "^(.+)\\(.+\\).*$")]

species_names[grep(x = species_names,pattern = "(^.+ .+ .+$)")]
gsub(x = species_names,pattern = "^\\w+ \\w+/.+$",replacement = "unknown plant")[grep(x = species_names,pattern = "^\\w+ \\w+/.+$")]


species_names[grep(x = species_names,pattern="(^\\w+ )\\w+/\\w+$")]
grep(x = species_names,pattern = "(^\\w+ \\w+\\..*$)")


# ×
species_names[grep(x = species_names,pattern = "\\.")]
species_names[grep(x = species_names,pattern = "unknown")]


species_names[6286]

length(unique(species_names))
length(species_names)

species_names[which(!(1:6299)%in%grep(x = species_names,pattern = "(^\\w+ \\w+\\.*$)"))]
