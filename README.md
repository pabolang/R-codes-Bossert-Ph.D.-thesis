# R-codes for the Ph.D. thesis ''Statistical structure and inference methods for discrete high-frequency observations of SPDEs in one and multiple space dimensions''

We provide the R-codes used to generate the plots within the Ph.D. thesis ''Statistical structure and inference methods for discrete high-frequency observations of SPDEs in one and multiple space dimensions''. The R-code for simulating the one-dimensional SPDE model as described by 

$$
\text{d} X_t(y) =  \Big(\vartheta_2\frac{\partial^2}{\partial y^2}X_t(y)+\vartheta_1\frac{\partial}{\partial y}X_t(y)+\vartheta_0X_t(y)\Big)\text{d} t+\sigma\text{d} B_t(y), (t,y)\in\[0,1\]\times \[0,1\]
$$ 

is available on the GitHub webpage [ParabolicSPDEs](https://github.com/pabolang/ParabolicSPDEs). The R-code for simulating the multi-dimensional model corresponding to the equation 

$$
\text{d} X_t(\textbf{y}) = A_\vartheta X_t(\textbf{y})\text{d} t+\sigma\text{d} B_t(\textbf{y}), (t,\textbf{y})\in[0,1]\times [0,1]^d
, $$ 

with 
$$A_\vartheta = \eta \sum_{l=1}^d \frac{\partial}{\partial y_l^2}+\sum_{l=1}^d \nu_l\frac{\partial}{\partial y_l}+\vartheta_0, $$ 
can be found on the webpage [SecondOrderSPDEMulti](https://github.com/pabolang/SecondOrderSPDEMulti). Since Figure 4.4 was created using the application ''Vectorworks'', we do not provide a corresponding code. For more information on the model and statistical inference, see [Efficient parameter estimation for parabolic SPDEs based on a log-linear model for realized volatilities](https://link.springer.com/article/10.1007/s42081-023-00192-4) for the one-dimensional case and [Parameter estimation for second-order SPDEs in multiple space dimensions](https://arxiv.org/abs/2310.17828) for the mulit-dimensional case. 
