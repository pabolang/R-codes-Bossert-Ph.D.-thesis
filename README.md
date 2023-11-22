# R-codes for the Ph.D. thesis ''Statistical structure and inference methods for discrete high-frequency observations of SPDEs in one and multiple space dimensions''

We provide the R-codes for the plots used in this thesis. The R-code for simulating the one-dimensional SPDE model as described by 
$$\left[
\begin{array}{ll}
\diff X_t(y) = \Big(\vartheta_2\frac{\partial^2}{\partial y^2}X_t(y)+\vartheta_1\frac{\partial}{\partial y}X_t(y)+\vartheta_0X_t(y)\Big)\diff t+\sigma\diff B_t(y), &(t,y)\in\R^+\times [y_{min},y_{max}]\\
X_0(y)=\xi(y),& y\in[y_{min},y_{max}]\\
X_t(y_{min})=X_t(y_{max})=0,& t\geq 0
\end{array}
\right] $$ 
is available on the GitHub webpage \href{https://github.com/pabolang/ParabolicSPDEs}{\texttt{ParabolicSPDEs}}\footnote{see: \url{https://github.com/pabolang/ParabolicSPDEs}}. For simulating the multi-dimensional model corresponding to the equation 
$$\left[
\begin{array}{ll}
\diff X_t(\textbf{y}) = A_\vartheta X_t(\textbf{y})\diff t+\sigma\diff B_t(\textbf{y}), &(t,\textbf{y})\in[0,1]\times [0,1]^d\\
X_0(\textbf{y})=\xi(\textbf{y}),& \textbf{y}\in[0,1]^d\\
X_t(\textbf{y})=0,& (t,\textbf{y})\in[0,1]\times\partial\,[0,1]^d
\end{array}
\right], $$ 
with 
$$A_\vartheta = \eta \sum_{l=1}^d \frac{\partial}{\partial y_l^2}+\sum_{l=1}^d \nu_l\frac{\partial}{\partial y_l}+\vartheta_0, $$ 
the R-Code can be found on the webpage \href{https://github.com/pabolang/SecondOrderSPDEMulti}{\texttt{SecondOrderSPDEMulti}}\footnote{see: \url{https://github.com/pabolang/SecondOrderSPDEMulti}}. Since Figure 4.4 was created using the application ''Vectorworks'', we do not provide a corresponding code.
