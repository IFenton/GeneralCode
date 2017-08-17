library(stringr)
par(mar = c(5.1, 10, 4.1, 2.1))
# italics and plain by hand
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("at the")))
axis(2, 1:10, labels = c(rep(expression(italic("italic")), 5), rep("plain", 5)), las = 1)

# second way of doing the same thing
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("at the")))
axis(2, 1:10, labels = eval(expression(c(rep(expression(italic("italic")), 5), rep("plain", 5)))), las = 1)

# italics from a names vector
names <- c(rep("italic", 5), rep("plain", 5))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, 1:10, labels = parse(text = paste("italic('", names, "')", sep = "")), las = 1)

# italics and plain from a names vector
names <- c(rep("italic", 5), rep("plain", 5))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, 1:10, labels = c(parse(text = paste("italic('", names[1:5], "')", sep = "")), names[6:10]), las = 1)

# italics and plain based on properties of a names vector
names <- c(rep("as italic", 5), rep("plain", 5))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, 1:10, labels = c(parse(text = paste("italic('", names[grep(" ", names)], "')", sep = "")), names[-grep(" ", names)]), las = 1)

# italics and plain based on properties of a names vector random order
names <- c(rep("as italic", 2), rep("plain", 3), rep("as italic", 3), rep("plain", 2))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, (1:10)[grep(" ", names)], labels = parse(text = paste("italic('", names[grep(" ", names)], "')", sep = "")), las = 1)
axis(2, (1:10)[-grep(" ", names)], labels = names[-grep(" ", names)], las = 1)


# italics, plain and a combination based on properties of a names vector
names <- c(rep("as italic", 2), "as italic plain", rep("plain", 5), rep("as italic", 2))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, (1:10)[str_count(names, " ") == 1], names[str_count(names, " ") == 1], las = 1, font = 3)
axis(2, (1:10)[str_count(names, " ") == 0], names[str_count(names, " ") == 0], las = 1)
axis(2, (1:10)[str_count(names, " ") == 2], labels = parse(text = paste(paste("italic('", gsub("^([^ ]* [^ ]*) (.*$)", "\\1", names[str_count(names, " ") == 2]), "')", sep = ""), gsub("^([^ ]* [^ ]*) (.*$)", "\\2", names[str_count(names, " ") == 2]), sep = "~")), las = 1)

gsub("^([^ ]* [^ ]*) (.*$)", "\\1", "as a plain string") # beginning
gsub("^([^ ]* [^ ]*) (.*$)", "\\2", "as a plain string") # end

# italics, plain and multiple combinations based on properties of a names vector
names <- c(rep("as italic", 2), rep("as italic plain", 2), rep("plain", 4), rep("as italic", 2))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, (1:10)[str_count(names, " ") == 1], names[str_count(names, " ") == 1], las = 1, font = 3)
axis(2, (1:10)[str_count(names, " ") == 0], names[str_count(names, " ") == 0], las = 1)
axis(2, (1:10)[str_count(names, " ") == 2], labels = parse(text = paste(paste("italic('", gsub("^([^ ]* [^ ]*) (.*$)", "\\1", names[str_count(names, " ") == 2]), "')", sep = ""), gsub("^([^ ]* [^ ]*) (.*$)", "\\2", names[str_count(names, " ") == 2]), sep = "~")), las = 1)

names <- c(rep("as italic", 2), "as italic plain", "as italic plain2", rep("plain", 4), rep("as italic", 2))
plot(1:10, 1:10, las = 1, yaxt = "n", main = expression(italic("italic")~"plain"))
axis(2, (1:10)[str_count(names, " ") == 1], names[str_count(names, " ") == 1], las = 1, font = 3)
axis(2, (1:10)[str_count(names, " ") == 0], names[str_count(names, " ") == 0], las = 1)
axis(2, (1:10)[str_count(names, " ") == 2], labels = parse(text = paste(paste("italic('", gsub("^([^ ]* [^ ]*) (.*$)", "\\1", names[str_count(names, " ") == 2]), "')", sep = ""), gsub("^([^ ]* [^ ]*) (.*$)", "\\2", names[str_count(names, " ") == 2]), sep = "~")), las = 1)

c(rep(expression(italic("italic")), 5), rep("plain", 5))
eval(expression(c(rep(expression(italic("italic")), 5), rep("plain", 5))))
parse(text = paste("italic('", names, "')", sep = ""))
c(parse(text = paste("italic('", names[1:5], "')", sep = "")), names[6:10])

