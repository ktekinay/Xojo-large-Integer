#tag Class
Class apmint
	#tag CompatibilityFlags = API2Only
	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub Constructor()
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns Me = 0
		  
		  Me.sign = 0
		  Me.used = 0
		  Return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub Constructor(num as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from apmint
		  Me.Sign = num.sign
		  Me.digits = num.digits
		  Me.used = num.used
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub Constructor(num as int64)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from int64
		  
		  var result As New apmint_module.apmint
		  apmint_from_int64(num, result)
		  Me.Sign = result.Sign
		  Me.used = result.used
		  Me.digits = result.digits
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub Constructor(num as String)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from String
		  
		  var result As New apmint_module.apmint
		  apmint_from_string(num, result)
		  Me.Sign = result.Sign
		  Me.used = result.used
		  Me.digits = result.digits
		  Return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub normalize()
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // removes any leading zeros
		  
		  If Me.Sign = 0 Then Return
		  
		  var mptr As ptr = Me.digits
		  var upper As Int64
		  
		  If Me.used <> 0 Then
		    
		    upper = Me.used - 8
		    While mptr.UInt64(upper) = 0
		      If upper = 0 Then
		        Me.Sign = 0
		        Me.used = 0
		        Exit
		      End If
		      upper = upper - 8
		      Me.used = Me.used - 8
		    Wend
		    
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_add(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // add two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_add(self, num, result)
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_addright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // add two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_add(self, num, result)
		  return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_and(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // bitwise and between two apmints
		  
		  var result As New apmint_module.apmint
		  apmint_bit_and(Self, num, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_andright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // bitwise and between two apmints
		  
		  var result As New apmint_module.apmint
		  apmint_bit_and(Self, num, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_compare(num as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // compare two apmints
		  
		  return apmint_compare(self, num)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub operator_convert(num as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from apmint
		  Me.Sign = num.sign
		  Me.digits = num.digits
		  Me.used = num.used
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub operator_convert(num as int64)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from int64
		  
		  var result As New apmint_module.apmint
		  apmint_from_int64(num, result)
		  Me.Sign = result.Sign
		  Me.used = result.used
		  Me.digits = result.digits
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub operator_convert(num as String)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from String
		  
		  var result As New apmint_module.apmint
		  apmint_from_string(num, result)
		  Me.Sign = result.Sign
		  Me.used = result.used
		  Me.digits = result.digits
		  Return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_divide(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide two apmints
		  
		  var quotient As New apmint_module.apmint
		  var remainder As New apmint_module.apmint
		  apmint_divide_remainder(Self, num, quotient, remainder)
		  Return quotient
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_divideright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide two apmints
		  
		  var quotient as New apmint_module.apmint
		  var remainder As New apmint_module.apmint
		  apmint_divide_remainder(num, self, quotient, remainder)
		  Return quotient
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_integerdivide(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide two apmints
		  
		  var quotient As New apmint_module.apmint
		  var remainder As New apmint_module.apmint
		  apmint_divide_remainder(Self, num, quotient, remainder)
		  Return quotient
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_integerdivideright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide two apmints
		  
		  var quotient as New apmint_module.apmint
		  var remainder As New apmint_module.apmint
		  apmint_divide_remainder(num, self, quotient, remainder)
		  Return quotient
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_modulo(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns self mod num
		  
		  var result As New apmint_module.apmint
		  apmint_modulo(Self, num, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_moduloright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns num mod self
		  
		  var result As New apmint_module.apmint
		  apmint_modulo(num, self, result)
		  Return result
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_multiply(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // multiply two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_multiply(self, num, result)
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_multiplyright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // multiply two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_multiply(self, num, result)
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_negate() As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns -self
		  
		  apmint_negate(self, self)
		  return self
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_or(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // bitwise or between two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_bit_or(self, num, result)
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_orright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // bitwise or between two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_bit_or(self, num, result)
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_power(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns self ^ num
		  
		  var result As New apmint_module.apmint
		  apmint_pow(Self, num, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_powerright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns num ^ self
		  
		  var result As New apmint_module.apmint
		  apmint_pow(num, Self, result)
		  Return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_subtract(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // subtract two apmints
		  
		  var result As New apmint_module.apmint
		  apmint_subtract(Self, num, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_subtractright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // subtract two apmints
		  
		  
		  var result As New apmint_module.apmint
		  apmint_subtract(num, self, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_xor(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // bitwise exclusive or between two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_bit_xor(self, num, result)
		  return result
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function operator_xorright(num as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // bitwise exclusive or between two apmints
		  
		  var result as new apmint_module.apmint
		  apmint_bit_xor(self, num, result)
		  return result
		  
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0, CompatibilityFlags = API2Only
		digits As memoryblock
	#tag EndProperty

	#tag Property, Flags = &h0, CompatibilityFlags = API2Only
		sign As Int64 = 0
	#tag EndProperty

	#tag Property, Flags = &h0, CompatibilityFlags = API2Only
		used As Int64
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sign"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Int64"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="used"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Int64"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
