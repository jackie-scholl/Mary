rm assembly_out/*
for filename in $(ls assembly)
do
	gcc assembly/$filename -o executable/$filename
	./executable/$filename >assembly_out/$filename.out
done
rm assembly/*
