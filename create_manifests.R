# Create the manifest file for the large wiki chunk files

dirs = c("AA", "AB", "AC", "AD");
ext_1 = as.matrix(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09"))
ext_2 = as.matrix(seq(10, 99));
ext = rbind(ext_1, ext_2);

chunk_paths = rep(0, length(ext)*length(dirs));
for (i in 1:4) {
  for (j in 1:100) {
    path = paste("/Users/admin/src/RabBIT/wiki-chunks/", dirs[i], "/wiki_", ext[j], sep="");
    chunk_paths[(i-1)*100 + j] = path;
  }
} 

chunk_paths = chunk_paths[1:(length(chunk_paths) -3)]

manifest<-file("/Users/admin/src/RabBIT/wiki_manifest.txt");
writeLines(chunk_paths, manifest)
close(manifest)
