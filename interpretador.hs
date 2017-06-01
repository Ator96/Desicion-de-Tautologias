interpretacionesVar :: Int -> [[Bool]]
interpretacionesVar 0  = [[]]
interpretacionesVar n  = 
	map (False:) bss ++ map (True:) bss
	where bss = interpretacionesVar (n -1)