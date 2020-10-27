#tag Class
Protected Class BigIntTests
Inherits TestGroup
	#tag Method, Flags = &h0
		Sub AdditionTest()
		  var bi as BigInt_MTC
		  
		  bi = new BigInt_MTC 
		  Assert.AreEqual "0", bi.ToString
		  
		  bi.Add 10
		  Assert.AreEqual "10", bi.ToString
		  
		  bi.Add new BigInt_MTC( 100 )
		  Assert.AreEqual "110", bi.ToString
		  
		  var kMaxInt64 as Int64 = 9223372036854775807
		  bi = new BigInt_MTC( kMaxInt64 )
		  bi.Add 1
		  Assert.AreEqual "9223372036854775808", bi.ToString
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ConstructorTest()
		  var bi as BigInt_MTC
		  
		  bi = new BigInt_MTC( 10 )
		  Assert.AreEqual "10", bi.ToString
		  
		  bi = new BigInt_MTC( "1000" )
		  Assert.AreEqual "1000", bi.ToString
		  
		  bi = new BigInt_MTC( -10 )
		  Assert.AreEqual "-10", bi.ToString
		  
		End Sub
	#tag EndMethod


End Class
#tag EndClass
