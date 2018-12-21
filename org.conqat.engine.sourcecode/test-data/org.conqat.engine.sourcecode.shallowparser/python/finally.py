class TheClass(
unittestEXAM.TestCase):

    #|#|4028870f3c7fcce0013c8059ab640404
    def testStartProgramWithoutRuns(self):
        climaticControlLib.unlockRS232 = unlockRS232Mock
        
        try:
            self.assertEqual(unlockRS232Mock.call_count, iRuns*3)
            
        finally:
            serialObject.write = writeOrigHandle

        return 
