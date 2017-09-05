type cluster={center : Local.data ref;nb_elems : int ref}
type chromosome = {r_fit:float;s_fit:float ref;
                   data:Local.data;clus:cluster option ref}


