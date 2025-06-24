# Run model1.R sequentially
nohup Rscript model1.R --n=500 --p=100 --nsim=500 --cores=5 --ratio=.95 --correlated=False --nsplits=1 --alpha=0 &> model_a0_n500.txt &
nohup Rscript model1.R --n=1000 --p=100 --nsim=500 --cores=5 --ratio=.96 --correlated=False --nsplits=1 --alpha=0 &> model_a0_n1000.txt &
nohup Rscript model1.R --n=2000 --p=100 --nsim=500 --cores=5 --ratio=.97 --correlated=False --nsplits=1 --alpha=0 &> model_a0_n2000.txt &

nohup Rscript model1.R --n=500 --p=100 --nsim=500 --cores=5 --ratio=.95 --correlated=False --nsplits=1 --alpha=.5 &> model_a5_n500.txt &
nohup Rscript model1.R --n=1000 --p=100 --nsim=500 --cores=5 --ratio=.96 --correlated=False --nsplits=1 --alpha=.5 &> model_a5_n1000.txt &
nohup Rscript model1.R --n=2000 --p=100 --nsim=500 --cores=5 --ratio=.97 --correlated=False --nsplits=1 --alpha=.5 &> model_a5_n2000.txt &

nohup Rscript model1.R --n=500 --p=100 --nsim=500 --cores=5 --ratio=.95 --correlated=False --nsplits=1 --alpha=1 &> model_a10_n500.txt &
nohup Rscript model1.R --n=1000 --p=100 --nsim=500 --cores=5 --ratio=.96 --correlated=False --nsplits=1 --alpha=1 &> model_a10_n1000.txt &
nohup Rscript model1.R --n=2000 --p=100 --nsim=500 --cores=5 --ratio=.97 --correlated=False --nsplits=1 --alpha=1 &> model_a10_n2000.txt &


nohup Rscript model1.R --n=500 --p=100 --nsim=500 --cores=5 --ratio=.95 --correlated=True --nsplits=1 --alpha=0 &> model_a0_n500_c.txt &
nohup Rscript model1.R --n=1000 --p=100 --nsim=500 --cores=5 --ratio=.96 --correlated=True --nsplits=1 --alpha=0 &> model_a0_n1000_c.txt &
nohup Rscript model1.R --n=2000 --p=100 --nsim=500 --cores=5 --ratio=.97 --correlated=True --nsplits=1 --alpha=0 &> model_a0_n2000_c.txt &

nohup Rscript model1.R --n=500 --p=100 --nsim=500 --cores=5 --ratio=.95 --correlated=True --nsplits=1 --alpha=.5 &> model_a5_n500_c.txt &
nohup Rscript model1.R --n=1000 --p=100 --nsim=500 --cores=5 --ratio=.96 --correlated=True --nsplits=1 --alpha=.5 &> model_a5_n1000_c.txt &
nohup Rscript model1.R --n=2000 --p=100 --nsim=500 --cores=5 --ratio=.97 --correlated=True --nsplits=1 --alpha=.5 &> model_a5_n2000_c.txt &

nohup Rscript model1.R --n=500 --p=100 --nsim=500 --cores=5 --ratio=.95 --correlated=True --nsplits=1 --alpha=1 &> model_a10_n500_c.txt &
nohup Rscript model1.R --n=1000 --p=100 --nsim=500 --cores=5 --ratio=.96 --correlated=True --nsplits=1 --alpha=1 &> model_a10_n1000_c.txt &
nohup Rscript model1.R --n=2000 --p=100 --nsim=500 --cores=5 --ratio=.97 --correlated=True --nsplits=1 --alpha=1 &> model_a10_n2000_c.txt &

