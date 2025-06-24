nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.5 --procedure="lasso" &> real_data_wine_lasso1.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.75 --procedure="lasso" &> real_data_wine_lasso2.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.9 --procedure="lasso" &> real_data_wine_lasso3.txt 

nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.5 --procedure="scad" &> real_data_wine_scad1.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.75 --procedure="scad" &> real_data_wine_scad2.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.9 --procedure="scad" &> real_data_wine_scad3.txt 

nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.5 --procedure="svr" &> real_data_wine_svr1.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.75 --procedure="svr" &> real_data_wine_svr2.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.9 --procedure="svr" &> real_data_wine_svr3.txt 

nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.5 --procedure="rf" &> real_data_wine_rf1.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.75 --procedure="rf" &> real_data_wine_rf2.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.9 --procedure="rf" &> real_data_wine_rf3.txt 

nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.5 --procedure="xgboost" &> real_data_wine_xgboost1.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.75 --procedure="xgboost" &> real_data_wine_xgboost2.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.9 --procedure="xgboost" &> real_data_wine_xgboost3.txt 

nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.5 --procedure="fnn" &> real_data_wine_fnn1.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.75 --procedure="fnn" &> real_data_wine_fnn2.txt 
nohup Rscript real_data_wine_mse.R --nrep=100 --ratio=.9 --procedure="fnn" &> real_data_wine_fnn3.txt 

