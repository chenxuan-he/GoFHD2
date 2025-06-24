# run model5.R sequentially
nohup Rscript model5.R --n=500 --alpha=0 --ratio=.95 --cores=10 &> model5_a0_n500.txt &
nohup Rscript model5.R --n=500 --alpha=1 --ratio=.95 --cores=10 &> model5_a1_n500.txt &
nohup Rscript model5.R --n=500 --alpha=2 --ratio=.95 --cores=10 &> model5_a2_n500.txt &

nohup Rscript model5.R --n=1000 --alpha=0 --ratio=.96 --cores=20 &> model5_a0_n1000.txt &
nohup Rscript model5.R --n=1000 --alpha=1 --ratio=.96 --cores=20 &> model5_a1_n1000.txt &
nohup Rscript model5.R --n=1000 --alpha=2 --ratio=.96 --cores=20 &> model5_a2_n1000.txt &

nohup Rscript model5.R --n=2000 --alpha=0 --ratio=.97 --cores=40 &> model5_a0_n2000.txt &
nohup Rscript model5.R --n=2000 --alpha=1 --ratio=.97 --cores=40 &> model5_a1_n2000.txt &
nohup Rscript model5.R --n=2000 --alpha=2 --ratio=.97 --cores=40 &> model5_a2_n2000.txt &

nohup Rscript model5.R --n=500 --p=200 --alpha=0 --ratio=.95 --cores=10 &> model5_a0_p200_n500.txt &
nohup Rscript model5.R --n=500 --p=200 --alpha=1 --ratio=.95 --cores=10 &> model5_a1_p200_n500.txt &
nohup Rscript model5.R --n=500 --p=200 --alpha=2 --ratio=.95 --cores=10 &> model5_a2_p200_n500.txt &

nohup Rscript model5.R --n=1000 --p=200 --alpha=0 --ratio=.96 --cores=20 &> model5_a0_p200_n1000.txt &
nohup Rscript model5.R --n=1000 --p=200 --alpha=1 --ratio=.96 --cores=20 &> model5_a1_p200_n1000.txt &
nohup Rscript model5.R --n=1000 --p=200 --alpha=2 --ratio=.96 --cores=20 &> model5_a2_p200_n1000.txt &

nohup Rscript model5.R --n=2000 --p=200 --alpha=0 --ratio=.97 --cores=40 &> model5_a0_p200_n2000.txt &
nohup Rscript model5.R --n=2000 --p=200 --alpha=1 --ratio=.97 --cores=40 &> model5_a1_p200_n2000.txt &
nohup Rscript model5.R --n=2000 --p=200 --alpha=2 --ratio=.97 --cores=40 &> model5_a2_p200_n2000.txt &
