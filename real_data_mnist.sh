# Run real_data_mnist_accuracy.R sequentially
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.5 --procedure=1 &> real_data_mnist_11.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.75 --procedure=1 &> real_data_mnist_12.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.9 --procedure=1 &> real_data_mnist_13.txt &

nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.5 --procedure=2 &> real_data_mnist_21.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.75 --procedure=2 &> real_data_mnist_22.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.9 --procedure=2 &> real_data_mnist_23.txt 

nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.5 --procedure=3 &> real_data_mnist_31.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.75 --procedure=3 &> real_data_mnist_32.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.9 --procedure=3 &> real_data_mnist_33.txt 

nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.5 --procedure=4 &> real_data_mnist_41.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.75 --procedure=4 &> real_data_mnist_42.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.9 --procedure=4 &> real_data_mnist_43.txt 

nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.5 --procedure=5 &> real_data_mnist_51.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.75 --procedure=5 &> real_data_mnist_52.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.9 --procedure=5 &> real_data_mnist_53.txt 

nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.5 --procedure=6 &> real_data_mnist_61.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.75 --procedure=6 &> real_data_mnist_62.txt &
nohup Rscript real_data_mnist_accuracy.R --nrep=100 --ratio=.9 --procedure=6 &> real_data_mnist_63.txt 
