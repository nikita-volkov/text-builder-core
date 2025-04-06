module Features (tests) where

import Control.Monad
import qualified Features.StrictBuilder as StrictBuilder
import qualified Features.StrictTextBuilder as StrictTextBuilder
import Numeric.Natural (Natural)
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import TextBuilderDev
import Util.TestTrees
import Prelude

tests :: [TestTree]
tests =
  [ testGroup "StrictBuilder" StrictBuilder.tests,
    testGroup "StrictTextBuilder" StrictTextBuilder.tests,
    testGroup "unsafeSeptets" $
      [ customGenMonoid do
          maxGenSize <- QuickCheck.getSize
          maxSize <- QuickCheck.chooseInt (0, maxGenSize)
          actualSize <- QuickCheck.chooseInt (0, maxSize)
          septets <-
            replicateM actualSize $
              fromIntegral <$> QuickCheck.chooseInt (0, 127)
          pure (unsafeSeptets maxSize septets)
      ],
    testGroup "unsafeReverseSeptets" $
      [ customGenMonoid do
          maxGenSize <- QuickCheck.getSize
          maxSize <- QuickCheck.chooseInt (0, maxGenSize)
          septets <-
            replicateM maxSize $
              fromIntegral <$> QuickCheck.chooseInt (0, 127)
          pure (unsafeReverseSeptets maxSize septets)
      ],
    testGroup "finiteBits" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int finiteBits
          ]
      ],
    testGroup "paddedFiniteBits" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int paddedFiniteBits
          ]
      ],
    testGroup "binary" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int binary
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer binary
          ]
      ],
    testGroup "unsignedBinary" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word unsignedBinary
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural unsignedBinary
          ]
      ],
    testGroup "decimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int decimal
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer decimal
          ]
      ],
    testGroup "unsignedDecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word unsignedDecimal
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural unsignedDecimal
          ]
      ],
    testGroup "fixedUnsignedDecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word (fixedUnsignedDecimal 42)
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural (fixedUnsignedDecimal 42)
          ]
      ],
    testGroup "thousandSeparatedDecimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int (thousandSeparatedDecimal ',')
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer (thousandSeparatedDecimal ',')
          ]
      ],
    testGroup "thousandSeparatedUnsignedDecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word (thousandSeparatedUnsignedDecimal ',')
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural (thousandSeparatedUnsignedDecimal ',')
          ]
      ],
    testGroup "dataSizeInBytesInDecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word dataSizeInBytesInDecimal
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural dataSizeInBytesInDecimal
          ]
      ],
    testGroup "hexadecimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int hexadecimal
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer hexadecimal
          ]
      ],
    testGroup "unsignedHexadecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word unsignedHexadecimal
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural unsignedHexadecimal
          ]
      ],
    testGroup "fixedDouble" $
      [ mapsToMonoid (fixedDouble 3)
      ],
    testGroup "doublePercent" $
      [ mapsToMonoid (doublePercent 3)
      ],
    testGroup "utcTimeInIso8601" $
      [ mapsToMonoid utcTimeInIso8601
      ]
  ]
