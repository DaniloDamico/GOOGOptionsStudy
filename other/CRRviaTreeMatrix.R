##### RIVEDERE
call_tree <- matrix(NA, nrow=N+1, ncol = N+1)
put_tree <- matrix(NA, nrow=N+1, ncol = N+1)

# values at the final node
for(i in 1:(N+1)){
  # call: Max [ (Sn − K), 0 ]
  call_tree[N+1, i] <- max(tree_matrix[N+1, i]-K, 0)
  
  # put: Max [ (K − Sn), 0 ]
  put_tree[N+1, i] <- max(K - tree_matrix[N+1, i], 0)
}

# backwards induction
for (i in N:1) {
  for (j in 1:i) {
    call_tree[i, j] <- round(exp(-r * delta_t) * (p_tilde * call_tree[i + 1, j + 1] + q_tilde * call_tree[i+1, j]), 2)
    put_tree[i, j] <- round(exp(-r * delta_t) * (p_tilde * put_tree[i + 1, j + 1] + q_tilde * put_tree[i+1, j]), 2)
  }
}

cat("\n")
cat("\tEstimated Call Option Value: (5.15)\t\t", call_tree[1,1], "\n")
cat("\tEstimated Put Option Value: (2.64)\t\t", put_tree[1,1], "\n")