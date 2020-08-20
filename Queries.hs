module Queries where
import Secuencias

order2notinOrder3 = filter
                      (\seq2 ->
                        not $ any
                          (\seq3 -> any (\idx -> idx == 0 || idx == 3 * 2^2) (infixIndices seq2 seq3))
                          nestedMarvellous233
                      )
                      nestedMarvellous223
                    where nestedMarvellous233 = nestedMarvellous2 3 3
                          nestedMarvellous223 = nestedMarvellous2 2 3

order2TimesInOrder3 = map
                      (\seq2 -> (
                        seq2,
                        foldr
                          (\seq (acc1, acc2) -> (acc1 + if elem 0 seq then 1 else 0, acc2 + if elem (3 * 2^2) seq then 1 else 0)) (0,0)
                          (map
                            (\seq3 -> filter (\idx -> idx == 0 || idx == 3 * 2^2) (infixIndices seq2 seq3))
                            nestedMarvellous233)
                      ) )
                      nestedMarvellous223
                    where nestedMarvellous233 = nestedMarvellous2 3 3
                          nestedMarvellous223 = nestedMarvellous2 2 3