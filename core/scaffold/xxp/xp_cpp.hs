import XXP

main = runXXP $ do cmake "{{experiment}}"
                   spawn "{{experiment}}"
