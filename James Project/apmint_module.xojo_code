#tag Module
Protected Module apmint_module
	#tag CompatibilityFlags = API2Only
	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_abs(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // output = abs(input)
		  
		  if input is output then
		    output.sign = output.sign * output.sign
		    return
		  else
		    apmint_copy(input, output)
		    output.sign = output.sign * output.sign
		    return
		  end if
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_add(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = input1 + input2 (overwrites output)
		  // output can be same object as input1 or input2
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  // if one input positive and one negative, subtract instead
		  If input1.Sign * input2.Sign = -1 Then
		    If input1.Sign = -1 Then
		      apmint_subtract_abs(input2, input1, output)
		      Return
		    Else
		      apmint_subtract_abs(input1, input2, output)
		      Return
		    End If
		  End If
		  
		  var input1_sign as int64 = input1.sign
		  var input2_sign as int64 = input2.sign
		  apmint_add_abs(input1, input2, output)
		  
		  If input1_Sign * input2_Sign = 0 Then
		    output.Sign = input1_Sign + input2_Sign
		  Else
		    output.Sign = input1_Sign
		  End If
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_add_abs(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns output = abs(input1) + abs(input2), overwrites output
		  
		  if output is nil then output = new apmint_module.apmint
		  
		  // handle case of one or other of the inputs equal to zero
		  If input1.Sign * input2.Sign = 0 Then
		    If input1.Sign = 0 And input2.Sign = 0 Then
		      output.Sign = 0
		      output.used = 0
		      Return
		    Elseif input1.Sign = 0 Then
		      apmint_copy(input2, output)
		      output.sign = 1
		      Return
		    Elseif input2.Sign = 0 Then
		      apmint_copy(input1, output)
		      output.sign = 1
		      Return
		    End If
		  End If
		  
		  output.Sign = 1
		  
		  // handle special case of both values of Uint64 size
		  If input1.used + input2.used = 16 Then
		    var ui1 As UInt64 = input1.digits.UInt64Value(0)
		    var ui2 As UInt64 = ui1 + input2.digits.UInt64Value(0)
		    If ui2 < ui1 Then ' there is a carry
		      If output.digits Is Nil Or output.digits.size < 16 Then apmint_grow(output, 16)
		      output.digits.UInt64Value(8) = &h1
		      output.used = 16
		    Else ' no carry
		      If output.digits Is Nil Or output.digits.size < 8 Then output.digits = New memoryblock(8)
		      output.used = 8
		    End If
		    output.digits.uint64Value(0) = ui2
		    Return
		  End If
		  
		  // make 'a' the shortest, 'b' the longest
		  var a, b As apmint_module.apmint
		  If input1.used > input2.used Then
		    a = input2
		    b = input1
		  Else
		    a = input1
		    b = input2
		  End If
		  
		  var asize As Int64 = a.used
		  var bsize As Int64 = b.used
		  If output.digits Is Nil Or output.digits.size < bsize + 8 Then apmint_grow(output, bsize + 8)
		  a.used = asize ' set used values for a and b in case output is one of them (overwritting input)
		  b.used = bsize
		  var rptr As ptr = output.digits
		  var aptr As ptr = a.digits
		  var bptr As ptr = b.digits
		  
		  #If limit_bytes Then
		    If bsize + 8 > apmint_module.max_bytes Then
		      var re As New RuntimeException
		      re.Message = "apmint_UnderOverflowErr"
		      Raise re
		    End If
		  #EndIf
		  
		  var dp As Int64
		  var carry, ui1, ui2 As UInt64
		  
		  // while both numbers have word in each column
		  While dp < asize
		    ui2 = aptr.UInt64(dp)
		    ui1 = ui2 + bptr.UInt64(dp) + carry
		    If ui1 < ui2 Or ((ui1 = ui2) And (carry = &h1)) Then
		      carry = &h1
		    Else
		      carry = &h0
		    End If
		    rptr.UInt64(dp) = ui1
		    dp = dp + 8
		  Wend
		  
		  // add those columns where there is b only
		  If bsize > asize Then
		    While dp < bsize
		      ui2 = bptr.UInt64(dp)
		      ui1 =  ui2 + carry
		      If ui1 < ui2 Then
		        carry = &h1
		      Else
		        carry = &h0
		      End If
		      rptr.UInt64(dp) = ui1
		      dp = dp + 8
		    Wend
		  End If
		  
		  rptr.UInt64(dp) = carry
		  output.used = bsize + 8
		  output.normalize
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_array_int32(Paramarray values as int32) As int32()
		  // creates int32 array
		  return values
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_array_int8(Paramarray values as int8) As int8()
		  // creates byte array
		  return values
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_array_uint32(Paramarray values as uint32) As uint32()
		  // creates uint32 array
		  return values
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_array_uint8(Paramarray values as uint8) As uint8()
		  // creates uint8 array
		  return values
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_and(input as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // output = bitwise and of input, input2; output is overwritten
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If input.Sign * input2.Sign = 0 Then
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  var min_size As Int64 = If (input.used < input2.used, input.used, input2.used)
		  
		  apmint_grow(output, min_size)
		  
		  var rp As Int64
		  var input_ptr As ptr = input.digits
		  var input2_ptr As ptr = input2.digits
		  var optr As ptr = output.digits
		  
		  While rp < min_size
		    optr.UInt64(rp) = input_ptr.UInt64(rp) And input2_ptr.UInt64(rp)
		    rp = rp + 8
		  Wend
		  
		  output.Sign = 1
		  output.normalize
		  Return 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_clear(input as apmint_module.apmint, bit_number as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // clears a bit in an apmint
		  // bit numbering begins with zero
		  
		  Static mask() As UInt8 = apmint_array_uint8(&hFE, &hFD, &hFB, &hF7, &hEF, &hDF, &hBF, &h7F)
		  
		  apmint_copy(input, output)
		  
		  var byte_number As Int64 = bit_number \ 8
		  
		  If bit_number < 0 Or output.Sign = 0 Or (byte_number + 1) > output.used Then Return
		  
		  output.digits.uint8value(byte_number) = output.digits.uint8value(byte_number) And mask(bit_number Mod 8)
		  output.normalize
		  
		  Return
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_clear_range(input as apmint_module.apmint, start_bit as int64, stop_bit as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // clears a bit range (from start_bit to stop_bit) in an apmint
		  // bit numbering begins with zero
		  
		  apmint_copy(input, output) ' start with input
		  
		  // check arguments
		  if start_bit < 0 or stop_bit < 0 or stop_bit < start_bit then return
		  
		  for ii as int64 = stop_bit DownTo start_bit
		    apmint_bit_clear(output, ii, output)
		  next
		  
		  Return
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_flip(input as apmint_module.apmint, bit_number as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // flip a bit in an apmint
		  // bit numbering begins with zero
		  
		  Static mask() As UInt8 = apmint_array_uint8(&h1, &h2, &h4, &h8, &h10, &h20, &h40, &h80)
		  
		  apmint_copy(Input, output)
		  
		  If bit_number < 0 Then Return
		  
		  var needed_word_size As Int64 = ((bit_number \ 64) + 1) * 8
		  
		  #If limit_bytes Then
		    If needed_word_size  > apmint_module.max_bytes Then
		      var re As New RuntimeException
		      re.Message = "apmint_UnderOverflowErr"
		      Raise re
		    End If
		  #EndIf
		  
		  If needed_word_size > output.used Then apmint_grow(output, needed_word_size, True) ' grow size if necessary
		  
		  var byte_number As Int64 = bit_number \ 8
		  
		  output.digits.uint8value(byte_number) = output.digits.uint8value(byte_number) Xor mask(bit_number Mod 8)
		  
		  If output.Sign = 0 Then output.Sign = 1
		  
		  output.normalize
		  
		  Return
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_flip_range(input as apmint_module.apmint, start_bit as int64, stop_bit as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // flips a bit range (from start_bit to stop_bit) in an apmint
		  // bit numbering begins with zero
		  
		  apmint_copy(Input, output) ' start with input
		  
		  // check arguments
		  If start_bit < 0 Or stop_bit < 0 Or stop_bit < start_bit Then Return
		  
		  For ii As Int64 = stop_bit DownTo start_bit
		    apmint_bit_flip(output, ii, output)
		  Next
		  
		  Return
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_or(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // output = bitwise or of input1, input2; output is overwritten
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  output.Sign = 1
		  
		  If input1.Sign = 0 Then
		    If input2.Sign = 0 Then
		      output.Sign = 0
		      output.used = 0
		      Return
		    Elseif input2 Is output Then
		      Return
		    Else
		      apmint_copy(input2, output)
		      Return
		    End If
		  Elseif input2.Sign = 0 Then
		    If input1 Is output Then
		      Return
		    Else
		      apmint_copy(input1, output)
		      Return
		    End If
		  End If
		  
		  // strategy: or words up to shorter length and then copy up to longer length
		  
		  var input1_used As Int64 = input1.used ' the grow call below may step on .used properties, if output is one of inputs
		  var input2_used As Int64 = input2.used
		  var max_size As Int64 = If (input1_used > input2_used, input1_used, input2_used)
		  apmint_grow(output, max_size)
		  
		  var short_size, long_size As Int64
		  var short_ptr, long_ptr As ptr
		  If input1_used < input2_used Then
		    short_size = input1_used
		    long_size = input2_used
		    short_ptr = input1.digits
		    long_ptr = input2.digits
		  Else
		    short_size = input2_used
		    long_size = input1_used
		    short_ptr = input2.digits
		    long_ptr = input1.digits
		  End If
		  
		  var rp As Int64
		  var rptr As ptr = output.digits
		  
		  While rp < short_size
		    rptr.UInt64(rp) = short_ptr.UInt64(rp) Or long_ptr.UInt64(rp)
		    rp = rp + 8
		  Wend
		  
		  While rp < long_size
		    rptr.UInt64(rp) = long_ptr.UInt64(rp)
		    rp = rp + 8
		  Wend
		  
		  Return 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_set(input as apmint_module.apmint, bit_number as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // sets a bit in an apmint
		  // bit numbering begins with zero
		  
		  Static mask() As UInt8 = apmint_array_uint8(&h1, &h2, &h4, &h8, &h10, &h20, &h40, &h80)
		  
		  apmint_copy(input, output)
		  
		  if bit_number < 0 then return
		  
		  var needed_word_size As Int64 = ((bit_number \ 64) + 1) * 8
		  
		  #If limit_bytes Then
		    If needed_word_size  > apmint_module.max_bytes Then
		      var re As New RuntimeException
		      re.Message = "apmint_UnderOverflowErr"
		      Raise re
		    End If
		  #EndIf
		  
		  if needed_word_size > output.used then apmint_grow(output, needed_word_size, true) ' grow size if necessary
		  
		  var byte_number As Int64 = bit_number \ 8
		  
		  output.digits.uint8value(byte_number) = output.digits.uint8value(byte_number) or mask(bit_number Mod 8)
		  
		  if output.sign = 0 then output.sign = 1
		  
		  Return
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_set_range(input as apmint_module.apmint, start_bit as int64, stop_bit as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // sets a bit range (from start_bit to stop_bit) in an apmint
		  // bit numbering begins with zero
		  
		  apmint_copy(input, output) ' start with input
		  
		  // check arguments
		  if start_bit < 0 or stop_bit < 0 or stop_bit < start_bit then return
		  
		  for ii as int64 = stop_bit DownTo start_bit
		    apmint_bit_set(output, ii, output)
		  next
		  
		  Return
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_bit_xor(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // output = bitwise xor of input1, input2; output is overwritten
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  output.Sign = 1
		  
		  If input1.Sign = 0 Then
		    If input2.Sign = 0 Then
		      output.Sign = 0
		      output.used = 0
		      Return
		    Elseif input2 Is output Then
		      Return
		    Else
		      apmint_copy(input2, output)
		      Return
		    End If
		  Elseif input2.Sign = 0 Then
		    If input1 Is output Then
		      Return
		    Else
		      apmint_copy(input1, output)
		      Return
		    End If
		  End If
		  
		  // strategy: or words up to shorter length and then copy up to longer length
		  
		  var input1_used As Int64 = input1.used ' the grow call below may step on .used properties, if output is one of inputs
		  var input2_used As Int64 = input2.used
		  var max_size As Int64 = If (input1_used > input2_used, input1_used, input2_used)
		  apmint_grow(output, max_size)
		  
		  var short_size, long_size As Int64
		  var short_ptr, long_ptr As ptr
		  If input1_used < input2_used Then
		    short_size = input1_used
		    long_size = input2_used
		    short_ptr = input1.digits
		    long_ptr = input2.digits
		  Else
		    short_size = input2_used
		    long_size = input1_used
		    short_ptr = input2.digits
		    long_ptr = input1.digits
		  End If
		  
		  var rp As Int64
		  var rptr As ptr = output.digits
		  
		  While rp < short_size
		    rptr.UInt64(rp) = short_ptr.UInt64(rp) Xor long_ptr.UInt64(rp)
		    rp = rp + 8
		  Wend
		  
		  While rp < long_size
		    rptr.UInt64(rp) = long_ptr.UInt64(rp)
		    rp = rp + 8
		  Wend
		  
		  output.normalize
		  Return 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_compare(num1 as apmint_module.apmint, num2 as apmint_module.apmint) As int64
		  // compare two apmints
		  
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // determines if one apmint is less than, greater than, or equal to  the other
		  // returns int64 = -1, 0, 1 for less than, equal and greater than
		  
		  If num1.Sign < num2.Sign Then Return -1
		  If num1.Sign > num2.Sign Then Return 1
		  If num1.Sign = 0 Then Return 0
		  If num1.Sign = 1 Then Return apmint_compare_abs(num1, num2)
		  Return apmint_compare_abs(num2, num1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_compare_abs(num1 as apmint_module.apmint, num2 as apmint_module.apmint) As int64
		  // compare two apmints
		  
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // determines if one apmint is less than, greater than, or equal to  the other
		  // returns int64 = -1, 0, 1 for less than, equal and greater than
		  // this num1thod compares word content, ignoring sign
		  
		  // check lengths
		  If num1.used > num2.used Then Return 1  ' longer is bigger
		  If num1.used < num2.used Then Return -1 ' shorter is smaller
		  if num1.used = 0 then return 0          ' both empty is equal
		  
		  var mptr As ptr = num1.digits
		  var nptr As ptr = num2.digits
		  
		  // length of num2ber is sanum1, proceed to compare word content
		  // finding a larger word will determine bigger num2ber
		  // finding a smaller word will determine smaller num2ber
		  // if all words equal, the num2bers are equal
		  
		  var upper As Int64 = num1.used - 8
		  
		  While upper >= 0
		    If mptr.UInt64(upper) <> nptr.UInt64(upper) Then
		      If mptr.UInt64(upper) > nptr.UInt64(upper) Then
		        Return 1
		      Else
		        Return -1
		      End If
		    End If
		    upper = upper - 8
		  Wend
		  
		  Return 0 ' words were all equal
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_copy(input as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // copy an apmint, returns new apmint
		  
		  var result as new apmint_module.apmint
		  apmint_copy(input, result)
		  return result
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_copy(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // copies input to output, output is overwritten
		  
		  if output is nil then output = new apmint_module.apmint ' catch nil object
		  
		  if input is output then return ' output already is input
		  
		  // if input nil or zero, just set output to zero
		  if input is nil or input.sign = 0 then
		    output.sign = 0
		    output.used = 0
		    return
		  end if
		  
		  // if output is too small to hold input, create a new memoryblock
		  if output.digits is nil or output.digits.size < input.used then
		    output.digits = new memoryblock(input.digits.size)
		  end if
		  
		  
		  // output is large enough to hold input, copy words over
		  var ip as ptr = input.digits
		  var op as ptr = output.digits
		  var offset as int64
		  
		  offset = input.used - 8
		  
		  while true
		    op.uint64(offset) = ip.uint64(offset)
		    if offset <= 0 then exit
		    offset = offset - 8
		  wend
		  
		  output.sign = input.sign
		  output.used = input.used
		  
		  return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_divide(dividend_in as apmint_module.apmint, divisor_in as apmint_module.apmint,   byref quotient as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide ampint by apmint returning both quotient and remainder
		  
		  Static base_32bit As UInt64 = &h100000000      ' 32 bit base
		  Static lo_mask_32bit As UInt64 = &hFFFFFFFF    ' 32-bit lo mask
		  Static U64_Zero As UInt64 = CType(0, UInt64)
		  Static U64_One As UInt64 = CType(1, UInt64)
		  Static dividend_working As New memoryblock(800)
		  Static divisor_working As New memoryblock(800)
		  Static div_prod As New memoryblock(800)
		  Static final_quotient As New memoryblock(800)
		  
		  If quotient Is Nil Then quotient = New apmint_module.apmint
		  
		  // below needed because input could be overwritten
		  var dividend_in_sign As Int64 = dividend_in.Sign
		  var divisor_in_sign As Int64 = divisor_in.Sign
		  
		  // test for special case of dividend = 0
		  If dividend_in_sign = 0 Then
		    apmint_from_uint64(&h0, quotient)
		    Return
		  End If
		  
		  // if divide by zero, raise exception
		  If divisor_in_sign = 0 Then
		    var re As New RuntimeException
		    re.Message = "apmint_DivideByZeroErr"
		    Raise re
		  End If
		  
		  // test for special case of both values of Uint64 size
		  If divisor_in.used + dividend_in.used = 16 Then
		    var uquotient As UInt64 = dividend_in.digits.Uint64value(0) \ divisor_in.digits.UInt64Value(0)
		    apmint_from_uint64(uquotient, quotient)
		    If quotient.Sign <> 0 Then quotient.Sign = dividend_in_sign * divisor_in_sign
		    Return
		  End If
		  
		  // test for special case of dividend <= to divisor
		  var icmp As Int64 = apmint_compare_abs(dividend_in, divisor_in)
		  If icmp = -1 Then
		    apmint_from_uint64(&h0, quotient)
		    Return
		  Elseif icmp = 0 Then
		    apmint_from_uint64(&h1, quotient)
		    quotient.Sign = dividend_in_sign * divisor_in_sign
		    Return
		  End If
		  
		  // remove trailing zero from dividend and divisor
		  var dividend_digits_size As Int64 = dividend_in.used
		  If dividend_in.digits.Int32Value(dividend_digits_size - 4) = 0 Then dividend_digits_size = dividend_digits_size - 4
		  var divisor_digits_size As Int64 = divisor_in.used
		  If divisor_in.digits.Int32Value(divisor_digits_size - 4) = 0 Then divisor_digits_size = divisor_digits_size - 4
		  
		  var dividend_ptr As ptr = dividend_in.digits
		  var divisor_ptr As ptr = divisor_in.digits
		  var quotient_ptr As ptr
		  var ii, jj, kk As Int64
		  
		  //// treat case of single digit divisor as special case
		  If divisor_digits_size = 4 Then
		    var quotienti64, remainderi64, divisori64 As UInt64
		    If dividend_digits_size = 4 Then
		      // one divisor digit, one dividend digit
		      quotienti64 = CType(dividend_ptr.UInt32(0), UInt64) \ CType(divisor_ptr.UInt32(0), UInt64)
		      apmint_from_int64(quotienti64, quotient)
		      quotient.Sign = dividend_in_sign * divisor_in_sign
		      Return
		    Else
		      // one divisor digit, two or more dividend digits
		      var dp As Int64 = dividend_digits_size - 4  ' pointer to last digit of dividend
		      var rp, qp As Int64
		      var quotient_array() As UInt64
		      var result_size As Int64 = dividend_digits_size \ 4
		      redim quotient_array(result_size - 1)
		      divisori64 = divisor_ptr.UInt32(0)
		      
		      While dp >= 0
		        remainderi64 = (remainderi64 * base_32bit) + CType(dividend_ptr.UInt32(dp), UInt64)
		        quotienti64 = remainderi64 \ divisori64
		        remainderi64 = remainderi64 - (quotienti64 * divisori64)
		        quotient_array(rp) = quotienti64
		        rp = rp + 1
		        dp = dp - 4
		      Wend
		      
		      While quotient_array(jj) = 0
		        jj = jj + 1
		      Wend
		      
		      var quotient_length As Int64 = (rp - jj) * 4
		      If quotient_length Mod 8 <> 0 Then quotient_length = quotient_length + 4
		      If quotient_length > quotient.used Then apmint_grow(quotient, quotient_length)
		      quotient.used = quotient_length
		      quotient_ptr = quotient.digits
		      ii = rp - 1
		      While ii >= jj
		        quotient_ptr.UInt32(qp) = quotient_array(ii)
		        qp = qp + 4
		        ii = ii - 1
		      Wend
		      quotient.Sign = dividend_in_sign * divisor_in_sign
		      Return
		      
		    End If
		  End If
		  //// end of single digit divisor case
		  
		  // copy dividend and divisor to working areas; divisor will be padded by a zero in front; msd first
		  var dividend_working_size As Int64 = dividend_digits_size * 2 + 8
		  If dividend_working.size < dividend_working_size Then
		    dividend_working = New memoryblock(dividend_working_size)
		  End If
		  var dividend_working_ptr As ptr = dividend_working
		  var ptr1, ptr2 As Int64
		  ptr1 = 8
		  ptr2 = dividend_digits_size - 4
		  While ptr2 >= 0
		    dividend_working_ptr.UInt32(ptr1) = dividend_ptr.UInt32(ptr2)
		    ptr1 = ptr1 + 8
		    ptr2 = ptr2 - 4
		  Wend
		  
		  var divisor_working_size As Int64 = divisor_digits_size * 2
		  If divisor_working.size < divisor_working_size Then
		    divisor_working = New memoryblock(divisor_working_size)
		  End If
		  var divisor_working_ptr As ptr = divisor_working
		  ptr1 = 0
		  ptr2 = divisor_digits_size - 4
		  While ptr2 >=0
		    divisor_working_ptr.UInt32(ptr1) = divisor_ptr.UInt32(ptr2)
		    ptr1 = ptr1 + 8
		    ptr2 = ptr2 - 4
		  Wend
		  
		  // normalize divisor and dividend to improve division guesses
		  var normalization_factor As UInt64 = base_32bit \ (divisor_working_ptr.UInt64(0) + U64_One)
		  var divisor_length As Int64  = divisor_working_size \ 8
		  var dividend_length As Int64 = dividend_working_size \ 8
		  var ui1, carry As UInt64
		  
		  If (normalization_factor <> &h1) Then
		    
		    // Multiply divisor and dividend by normalization factor
		    For ii= (divisor_length-1) * 8 DownTo 0 Step 8
		      ui1 = divisor_working_ptr.UInt64(ii)
		      ui1 = (ui1 * normalization_factor) + carry
		      carry = ui1 \ base_32bit
		      divisor_working_ptr.UInt64(ii) = ui1 And lo_mask_32bit
		    Next
		    
		    carry = 0
		    For ii= (dividend_length-1) * 8 DownTo 0 Step 8
		      ui1 = dividend_working_ptr.UInt64(ii)
		      ui1 = (ui1 * normalization_factor) + carry
		      carry = ui1 \ base_32bit
		      dividend_working_ptr.UInt64(ii) = ui1 And lo_mask_32bit
		    Next
		    
		  End If
		  
		  var divisor1d As UInt64 = divisor_working_ptr.UInt64(0) ' divisor first word used by division guesses
		  
		  var divisions_remaining As Int64 = dividend_length - divisor_length   ' how many divisions left
		  var dividend2d As UInt64                                              ' will hold two dividend digits
		  var division_guess As UInt64                                          ' a guess at a quotient digit
		  var guess_good As Boolean                                             ' was our quess good
		  var borrow As Boolean                                                 ' detect borrows in subtraction
		  var dividend_offset As Int64                                          ' increased when dividend is shifted
		  If div_prod.size < dividend_working_size Then
		    div_prod = New memoryblock(dividend_working_size)   ' temp working area used to check division
		  End If
		  var div_prod_ptr As ptr = div_prod
		  If final_quotient.size < dividend_working_size Then
		    final_quotient = New memoryblock(dividend_working_size) ' store quotient words as we get them
		  End If
		  var final_quotient_ptr As ptr = final_quotient
		  var final_quotient_offset As Int64
		  
		  
		  While (True)
		    
		    // Get the two digits from working remainder for quess
		    dividend2d = dividend_working_ptr.UInt64(dividend_offset) * base_32bit + dividend_working_ptr.UInt64(dividend_offset + 8)
		    
		    division_guess = dividend2d \ divisor1d  ' guess the next quotient digit
		    If division_guess > lo_mask_32bit Then division_guess = lo_mask_32bit
		    
		    If (division_guess <> 0) Then
		      
		      // multiply divisor by guess
		      carry = 0
		      For ii = (divisor_length-1) * 8 DownTo 0 Step 8
		        jj = ii + 8
		        ui1 = (divisor_working_ptr.UInt64(ii) * division_guess) + carry
		        carry = ui1 \ base_32bit
		        div_prod_ptr.UInt64(jj) = ui1 And lo_mask_32bit
		      Next
		      div_prod_ptr.UInt64(0) = carry
		      
		      // check to see if guess is correct, if not subtract one from guess and try again (and again)
		      While True
		        guess_good = True
		        jj = divisor_length * 8
		        For ii = 0 To jj Step 8
		          kk = ii + dividend_offset
		          If dividend_working_ptr.UInt64(kk) <>  div_prod_ptr.UInt64(ii) Then
		            If dividend_working_ptr.UInt64(kk) < div_prod_ptr.UInt64(ii) Then
		              guess_good = False
		              Exit
		            Else
		              Exit
		            End If
		          End If
		        Next
		        
		        If  guess_good = True Then Exit
		        division_guess = division_guess - 1      ' bad guesses are almost always off by one
		        
		        // multiply divisor by guess again
		        carry = 0
		        For ii = (divisor_length-1) * 8 DownTo 0 Step 8
		          jj = ii + 8
		          ui1 = (divisor_working_ptr.UInt64(ii) * division_guess) + carry
		          carry = ui1 \ base_32bit
		          div_prod_ptr.UInt64(jj) = ui1 And lo_mask_32bit
		        Next
		        div_prod_ptr.UInt64(0) = carry
		        
		      Wend
		      
		      // subtract divisor * guess from working remainder (dividend_working) for new working remainder
		      borrow = False
		      jj = divisor_length * 8
		      For ii = jj DownTo 0 Step 8
		        kk = ii + dividend_offset
		        If borrow Then
		          If (dividend_working_ptr.UInt64(kk) = U64_Zero) Then
		            dividend_working_ptr.UInt64(kk) = lo_mask_32bit
		          Else
		            dividend_working_ptr.UInt64(kk) = dividend_working_ptr.UInt64(kk) - U64_One
		            borrow = False
		          End If
		        End If
		        
		        dividend_working_ptr.Int64(kk) = dividend_working_ptr.Int64(kk) - div_prod_ptr.Int64(ii)
		        If (dividend_working_ptr.Int64(kk) < 0) Then
		          dividend_working_ptr.Int64(kk) = dividend_working_ptr.Int64(kk) + base_32bit
		          borrow = True
		        End If
		      Next
		    End If
		    
		    // have another digit for the quotient
		    final_quotient_ptr.UInt64(final_quotient_offset) = division_guess
		    final_quotient_offset = final_quotient_offset + 8
		    
		    // if the required number of divisions have been done, then exit loop
		    divisions_remaining = divisions_remaining - 1
		    If divisions_remaining = 0 Then Exit While
		    
		    // shift dividend to the left
		    dividend_offset = dividend_offset + 8
		  Wend
		  
		  // now place final quotient in the bigint (words reversed)
		  jj = 0
		  While final_quotient_ptr.UInt64(jj) = &h0
		    jj = jj + 8
		  Wend
		  
		  ptr1 = 0
		  var quotient_length As Int64 = (final_quotient_offset - jj) \ 2
		  If quotient_length Mod 8 <> 0 Then quotient_length = quotient_length + 4
		  quotient.digits = New memoryblock(quotient_length)
		  quotient.used = quotient_length
		  quotient_ptr = quotient.digits
		  ii = final_quotient_offset - 8
		  While ii >= jj
		    quotient_ptr.UInt32(ptr1) = final_quotient_ptr.UInt32(ii)
		    ptr1 = ptr1 + 4
		    ii = ii - 8
		  Wend
		  While ptr1 < quotient.used    ' clear remaining used space
		    quotient_ptr.UInt32(ptr1) = 0
		    ptr1 = ptr1 + 4
		  Wend
		  quotient.Sign = dividend_in_sign * divisor_in_sign
		  
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_divide_by2(num as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = num \ 2 (shift right one bit), output is overwritten
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If num.Sign = 0 Then
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  var output_size As Int64 = num.used
		  output.Sign = num.Sign
		  If output.used < output_size Then apmint_grow(output, output_size)
		  output.used = output_size
		  
		  var rptr As ptr = output.digits
		  var nptr As ptr = num.digits
		  var offset As Int64 = num.used - 8
		  var ui1, lower_bit As UInt64
		  
		  While True
		    ui1 = nptr.UInt64(offset)
		    rptr.UInt64(offset) = (ui1 \ &h2) Or lower_bit
		    lower_bit = (ui1 And &h1) * &h8000000000000000
		    If offset <= 0 Then Exit
		    offset = offset - 8
		  Wend
		  
		  output.normalize
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_divide_newton(dividend_in as apmint_module.apmint, divisor_in as apmint_module.apmint) As pair
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide ampint by apmint returning both quotient and remainder
		  // uses a newton iteration
		  
		  var quotient As New apmint_module.apmint
		  var remainder As New apmint_module.apmint
		  
		  // test for special case of dividend = 0
		  If dividend_in.Sign = 0 Then Return quotient : remainder
		  
		  // if divide by zero, raise exception
		  If divisor_in.Sign = 0 Then
		    var re As New RuntimeException
		    re.Message = "apmint_DivideByZeroErr"
		    Raise re
		  End If
		  
		  var dividend, divisor as apmint_module.apmint
		  apmint_abs(dividend_in, dividend)
		  apmint_abs(divisor_in, divisor)
		  
		  //// test for special case of dividend <= to divisor
		  var icmp As Int64 = apmint_compare(dividend, divisor)
		  If icmp = -1 Then
		    remainder = dividend_in
		    Return quotient : remainder
		  Elseif icmp = 0 Then
		    quotient = 1
		    quotient.Sign = dividend_in.Sign * divisor_in.Sign
		    Return quotient : remainder
		  End If
		  
		  var k As Int64 = apmint_get_bit_length(dividend) + apmint_get_bit_length(divisor)
		  var one As apmint_module.apmint = 1
		  var pow2 As apmint_module.apmint
		  apmint_shift_left(one, k+1, pow2)
		  var x As apmint_module.apmint = dividend - divisor
		  var lastx As New apmint_module.apmint
		  var lastlastx As New apmint_module.apmint
		  
		  While True
		    
		    x = x * (pow2 - x * divisor)
		    apmint_shift_right(x, k, x)
		    If x = lastx Or x = lastlastx Then Exit
		    lastlastx = lastx
		    lastx = x
		    
		  Wend
		  quotient = dividend * x
		  apmint_shift_right(quotient, k, quotient)
		  remainder = dividend - (quotient * divisor)
		  If remainder >= divisor Then
		    quotient = quotient + 1
		    remainder = remainder - divisor
		  End If
		  quotient.Sign = dividend_in.Sign * divisor_in.Sign
		  If remainder.Sign <> 0 Then remainder.Sign = dividend_in.Sign
		  Return quotient : remainder
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_divide_remainder(dividend_in as apmint_module.apmint, divisor_in as apmint_module.apmint,   byref quotient as apmint_module.apmint,  byref remainder as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide ampint by apmint returning both quotient and remainder
		  
		  Static base_32bit As UInt64 = &h100000000      ' 32 bit base
		  Static lo_mask_32bit As UInt64 = &hFFFFFFFF  ' 32-bit lo mask
		  Static U64_Zero As UInt64 = CType(0, UInt64)
		  Static U64_One As UInt64 = CType(1, UInt64)
		  Static dividend_working As New memoryblock(800)
		  Static divisor_working As New memoryblock(800)
		  Static div_prod As New memoryblock(800)
		  Static final_quotient As New memoryblock(800)
		  
		  If quotient Is Nil Then quotient = New apmint_module.apmint
		  If remainder Is Nil Then remainder = New apmint_module.apmint
		  
		  // below needed because input could be overwritten
		  var dividend_in_sign As Int64 = dividend_in.Sign
		  var divisor_in_sign As Int64 = divisor_in.Sign
		  
		  // test for special case of dividend = 0
		  If dividend_in_sign = 0 Then
		    apmint_from_uint64(&h0, quotient)
		    apmint_from_uint64(&h0, remainder)
		    Return
		  End If
		  
		  // if divide by zero, raise exception
		  If divisor_in_sign = 0 Then
		    var re As New RuntimeException
		    re.Message = "apmint_DivideByZeroErr"
		    Raise re
		  End If
		  
		  // test for special case of both values of Uint64 size
		  If divisor_in.used + dividend_in.used = 16 Then
		    var uquotient As UInt64 = dividend_in.digits.Uint64value(0) \ divisor_in.digits.UInt64Value(0)
		    apmint_from_uint64(uquotient, quotient)
		    apmint_from_uint64(dividend_in.digits.UInt64Value(0) - uquotient * divisor_in.digits.Uint64value(0), remainder)
		    If quotient.Sign <> 0 Then quotient.Sign = dividend_in_sign * divisor_in_sign
		    If remainder.Sign <> 0 Then remainder.Sign = dividend_in_sign
		    Return
		  End If
		  
		  // test for special case of dividend <= to divisor
		  var icmp As Int64 = apmint_compare_abs(dividend_in, divisor_in)
		  If icmp = -1 Then
		    apmint_from_uint64(&h0, quotient)
		    apmint_copy(dividend_in, remainder)
		    Return
		  Elseif icmp = 0 Then
		    apmint_from_uint64(&h1, quotient)
		    apmint_from_uint64(&h0, remainder)
		    quotient.Sign = dividend_in_sign * divisor_in_sign
		    Return
		  End If
		  
		  // remove trailing zero from dividend and divisor
		  var dividend_digits_size As Int64 = dividend_in.used
		  If dividend_in.digits.Int32Value(dividend_digits_size - 4) = 0 Then dividend_digits_size = dividend_digits_size - 4
		  var divisor_digits_size As Int64 = divisor_in.used
		  If divisor_in.digits.Int32Value(divisor_digits_size - 4) = 0 Then divisor_digits_size = divisor_digits_size - 4
		  
		  var dividend_ptr As ptr = dividend_in.digits
		  var divisor_ptr As ptr = divisor_in.digits
		  var quotient_ptr As ptr
		  var ii, jj, kk As Int64
		  
		  //// treat case of single digit divisor as special case
		  If divisor_digits_size = 4 Then
		    var quotienti64, remainderi64, divisori64 As UInt64
		    If dividend_digits_size = 4 Then
		      // one divisor digit, one dividend digit
		      quotienti64 = CType(dividend_ptr.UInt32(0), UInt64) \ CType(divisor_ptr.UInt32(0), UInt64)
		      remainderi64 = CType(dividend_ptr.UInt32(0), UInt64) - (quotienti64 * CType(divisor_ptr.UInt32(0), UInt64))
		      apmint_from_int64(quotienti64, quotient)
		      quotient.Sign = dividend_in_sign * divisor_in_sign
		      apmint_from_int64(remainderi64, remainder)
		      If remainder.Sign <> 0 Then remainder.Sign = dividend_in_sign
		      Return
		    Else
		      // one divisor digit, two or more dividend digits
		      var dp As Int64 = dividend_digits_size - 4  ' pointer to last digit of dividend
		      var rp, qp As Int64
		      var quotient_array() As UInt64
		      var result_size As Int64 = dividend_digits_size \ 4
		      redim quotient_array(result_size - 1)
		      divisori64 = divisor_ptr.UInt32(0)
		      
		      While dp >= 0
		        remainderi64 = (remainderi64 * base_32bit) + CType(dividend_ptr.UInt32(dp), UInt64)
		        quotienti64 = remainderi64 \ divisori64
		        remainderi64 = remainderi64 - (quotienti64 * divisori64)
		        quotient_array(rp) = quotienti64
		        rp = rp + 1
		        dp = dp - 4
		      Wend
		      apmint_from_int64(remainderi64, remainder)
		      If remainder.Sign <> 0 Then remainder.Sign = dividend_in_sign
		      
		      While quotient_array(jj) = 0
		        jj = jj + 1
		      Wend
		      
		      var quotient_length As Int64 = (rp - jj) * 4
		      If quotient_length Mod 8 <> 0 Then quotient_length = quotient_length + 4
		      If quotient_length > quotient.used Then apmint_grow(quotient, quotient_length)
		      quotient.used = quotient_length
		      quotient_ptr = quotient.digits
		      ii = rp - 1
		      While ii >= jj
		        quotient_ptr.UInt32(qp) = quotient_array(ii)
		        qp = qp + 4
		        ii = ii - 1
		      Wend
		      quotient.Sign = dividend_in_sign * divisor_in_sign
		      Return
		      
		    End If
		  End If
		  //// end of single digit divisor case
		  
		  // copy dividend and divisor to working areas; divisor will be padded by a zero in front; msd first
		  var dividend_working_size As Int64 = dividend_digits_size * 2 + 8
		  If dividend_working.size < dividend_working_size Then
		    dividend_working = New memoryblock(dividend_working_size)
		  End If
		  var dividend_working_ptr As ptr = dividend_working
		  var ptr1, ptr2 As Int64
		  ptr1 = 8
		  ptr2 = dividend_digits_size - 4
		  While ptr2 >= 0
		    dividend_working_ptr.UInt32(ptr1) = dividend_ptr.UInt32(ptr2)
		    ptr1 = ptr1 + 8
		    ptr2 = ptr2 - 4
		  Wend
		  
		  var divisor_working_size As Int64 = divisor_digits_size * 2
		  If divisor_working.size < divisor_working_size Then
		    divisor_working = New memoryblock(divisor_working_size)
		  End If
		  var divisor_working_ptr As ptr = divisor_working
		  ptr1 = 0
		  ptr2 = divisor_digits_size - 4
		  While ptr2 >=0
		    divisor_working_ptr.UInt32(ptr1) = divisor_ptr.UInt32(ptr2)
		    ptr1 = ptr1 + 8
		    ptr2 = ptr2 - 4
		  Wend
		  
		  // normalize divisor and dividend to improve division guesses; must divide out of final remainder at end
		  var normalization_factor As UInt64 = base_32bit \ (divisor_working_ptr.UInt64(0) + U64_One)
		  var divisor_length As Int64  = divisor_working_size \ 8
		  var dividend_length As Int64 = dividend_working_size \ 8
		  var ui1, ui2, carry As UInt64
		  
		  If (normalization_factor <> &h1) Then
		    
		    // Multiply divisor and dividend by normalization factor
		    For ii= (divisor_length-1) * 8 DownTo 0 Step 8
		      ui1 = divisor_working_ptr.UInt64(ii)
		      ui1 = (ui1 * normalization_factor) + carry
		      carry = ui1 \ base_32bit
		      divisor_working_ptr.UInt64(ii) = ui1 And lo_mask_32bit
		    Next
		    
		    carry = 0
		    For ii= (dividend_length-1) * 8 DownTo 0 Step 8
		      ui1 = dividend_working_ptr.UInt64(ii)
		      ui1 = (ui1 * normalization_factor) + carry
		      carry = ui1 \ base_32bit
		      dividend_working_ptr.UInt64(ii) = ui1 And lo_mask_32bit
		    Next
		    
		  End If
		  
		  var divisor1d As UInt64 = divisor_working_ptr.UInt64(0) ' divisor first word used by division guesses
		  
		  var divisions_remaining As Int64 = dividend_length - divisor_length   ' how many divisions left
		  var dividend2d As UInt64                                              ' will hold two dividend digits
		  var division_guess As UInt64                                          ' a guess at a quotient digit
		  var guess_good As Boolean                                             ' was our quess good
		  var borrow As Boolean                                                 ' detect borrows in subtraction
		  var dividend_offset As Int64                                          ' increased when dividend is shifted
		  If div_prod.size < dividend_working_size Then
		    div_prod = New memoryblock(dividend_working_size)   ' temp working area used to check division
		  End If
		  var div_prod_ptr As ptr = div_prod
		  If final_quotient.size < dividend_working_size Then
		    final_quotient = New memoryblock(dividend_working_size) ' store quotient words as we get them
		  End If
		  var final_quotient_ptr As ptr = final_quotient
		  var final_quotient_offset As Int64
		  
		  
		  While (True)
		    
		    // Get the two digits from working remainder for quess
		    dividend2d = dividend_working_ptr.UInt64(dividend_offset) * base_32bit + dividend_working_ptr.UInt64(dividend_offset + 8)
		    
		    division_guess = dividend2d \ divisor1d  ' guess the next quotient digit
		    If division_guess > lo_mask_32bit Then division_guess = lo_mask_32bit
		    
		    If (division_guess <> 0) Then
		      
		      // multiply divisor by guess
		      carry = 0
		      For ii = (divisor_length-1) * 8 DownTo 0 Step 8
		        jj = ii + 8
		        ui1 = (divisor_working_ptr.UInt64(ii) * division_guess) + carry
		        carry = ui1 \ base_32bit
		        div_prod_ptr.UInt64(jj) = ui1 And lo_mask_32bit
		      Next
		      div_prod_ptr.UInt64(0) = carry
		      
		      // check to see if guess is correct, if not subtract one from guess and try again (and again)
		      While True
		        guess_good = True
		        jj = divisor_length * 8
		        For ii = 0 To jj Step 8
		          kk = ii + dividend_offset
		          If dividend_working_ptr.UInt64(kk) <>  div_prod_ptr.UInt64(ii) Then
		            If dividend_working_ptr.UInt64(kk) < div_prod_ptr.UInt64(ii) Then
		              guess_good = False
		              Exit
		            Else
		              Exit
		            End If
		          End If
		        Next
		        
		        If  guess_good = True Then Exit
		        division_guess = division_guess - 1      ' bad guesses are almost always off by one
		        
		        // multiply divisor by guess again
		        carry = 0
		        For ii = (divisor_length-1) * 8 DownTo 0 Step 8
		          jj = ii + 8
		          ui1 = (divisor_working_ptr.UInt64(ii) * division_guess) + carry
		          carry = ui1 \ base_32bit
		          div_prod_ptr.UInt64(jj) = ui1 And lo_mask_32bit
		        Next
		        div_prod_ptr.UInt64(0) = carry
		        
		      Wend
		      
		      // subtract divisor * guess from working remainder (dividend_working) for new working remainder
		      borrow = False
		      jj = divisor_length * 8
		      For ii = jj DownTo 0 Step 8
		        kk = ii + dividend_offset
		        If borrow Then
		          If (dividend_working_ptr.UInt64(kk) = U64_Zero) Then
		            dividend_working_ptr.UInt64(kk) = lo_mask_32bit
		          Else
		            dividend_working_ptr.UInt64(kk) = dividend_working_ptr.UInt64(kk) - U64_One
		            borrow = False
		          End If
		        End If
		        
		        dividend_working_ptr.Int64(kk) = dividend_working_ptr.Int64(kk) - div_prod_ptr.Int64(ii)
		        If (dividend_working_ptr.Int64(kk) < 0) Then
		          dividend_working_ptr.Int64(kk) = dividend_working_ptr.Int64(kk) + base_32bit
		          borrow = True
		        End If
		      Next
		    End If
		    
		    // have another digit for the quotient
		    final_quotient_ptr.UInt64(final_quotient_offset) = division_guess
		    final_quotient_offset = final_quotient_offset + 8
		    
		    // if the required number of divisions have been done, then exit loop
		    divisions_remaining = divisions_remaining - 1
		    If divisions_remaining = 0 Then Exit While
		    
		    // shift dividend to the left
		    dividend_offset = dividend_offset + 8
		  Wend
		  
		  // dividend_working is now final remainder (divide out normalization_factor)
		  If normalization_factor <> &h1 Then
		    ui1 = 0
		    ii = divisor_length * 8 + dividend_offset
		    For jj = dividend_offset To ii Step 8
		      ui1 = (ui1 * base_32bit) + dividend_working_ptr.UInt64(jj)
		      ui2 = ui1 \ normalization_factor
		      ui1 = ui1 - (ui2 * normalization_factor)
		      dividend_working_ptr.UInt64(jj) = ui2
		    Next
		  End If
		  
		  // now place final quotient and remainder in the bigints (words reversed)
		  jj = 0
		  While final_quotient_ptr.UInt64(jj) = &h0
		    jj = jj + 8
		  Wend
		  
		  ptr1 = 0
		  var quotient_length As Int64 = (final_quotient_offset - jj) \ 2
		  If quotient_length Mod 8 <> 0 Then quotient_length = quotient_length + 4
		  quotient.digits = New memoryblock(quotient_length)
		  quotient.used = quotient_length
		  quotient_ptr = quotient.digits
		  ii = final_quotient_offset - 8
		  While ii >= jj
		    quotient_ptr.UInt32(ptr1) = final_quotient_ptr.UInt32(ii)
		    ptr1 = ptr1 + 4
		    ii = ii - 8
		  Wend
		  While ptr1 < quotient.used    ' clear remaining used space
		    quotient_ptr.UInt32(ptr1) = 0
		    ptr1 = ptr1 + 4
		  Wend
		  quotient.Sign = dividend_in_sign * divisor_in_sign
		  
		  var first_remainder_digit As Int64 = -1
		  ii = divisor_length * 8 + dividend_offset
		  For jj = dividend_offset To ii Step 8
		    If dividend_working_ptr.UInt64(jj) <> 0 Then
		      first_remainder_digit = jj
		      Exit
		    End If
		  Next
		  
		  If first_remainder_digit <> -1 Then
		    ptr1 = 0
		    
		    var remainder_length As Int64 = (ii - first_remainder_digit) \ 2 + 4
		    If remainder_length Mod 8 <> 0 Then remainder_length = remainder_length + 4
		    If remainder_length > remainder.used Then apmint_grow(remainder, remainder_length)
		    remainder.used = remainder_length
		    var remainder_ptr As ptr = remainder.digits
		    For jj = ii DownTo first_remainder_digit Step 8
		      remainder_ptr.UInt32(ptr1) = dividend_working_ptr.UInt64(jj)
		      ptr1 = ptr1 + 4
		    Next
		    While ptr1 < remainder.used     ' clear remaining used space
		      remainder_ptr.UInt32(ptr1) = 0
		      ptr1 = ptr1 + 4
		    Wend
		    remainder.Sign = dividend_in_sign
		  End If
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_egcd(aa as apmint_module.apmint, bb as apmint_module.apmint, byref g as apmint_module.apmint, byref xx as apmint_module.apmint, byref yy as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // extended GCD algorithm
		  // returns apmint array (g, x, y), such that ax + by = g = gcd(a, b)
		  
		  Static zero As New apmint_module.apmint
		  Static one As apmint_module.apmint = 1
		  var a as new apmint_module.apmint
		  var b as new apmint_module.apmint
		  var q as new apmint_module.apmint
		  var r as new apmint_module.apmint
		  var x as new apmint_module.apmint
		  var x1 as new apmint_module.apmint
		  var y as new apmint_module.apmint
		  var y1 as new apmint_module.apmint
		  var t1 as new apmint_module.apmint
		  
		  apmint_copy(aa, a)
		  a.sign = a.sign * a.sign
		  apmint_copy(bb, b)
		  b.sign = b.sign * b.sign
		  
		  apmint_copy(zero, x1)
		  apmint_copy(one, y1)
		  apmint_copy(one, x)
		  apmint_copy(zero, y)
		  
		  While b.Sign <> 0
		    // a , b = b , a % b
		    apmint_divide_remainder(a, b, q, r)
		    apmint_swap(a, b)
		    apmint_copy(r, b)
		    // x, x1 = x1, x - (q * x1)
		    apmint_multiply(q, x1, t1)
		    apmint_subtract(x, t1, t1)
		    apmint_swap(x, x1)
		    apmint_copy(t1, x1)
		    // y, y1 = y1, y - (q * y1)
		    apmint_multiply(q, y1, t1)
		    apmint_subtract(y, t1, t1)
		    apmint_swap(y, y1)
		    apmint_copy(t1, y1)
		  Wend
		  
		  apmint_copy(a, g)                           'g of g = gcd(a, b)
		  If aa.Sign = -1 Then x.sign = -x.sign
		  If bb.Sign = -1 Then y.sign = -y.sign
		  apmint_copy(x, xx)                          'x of ax + by = g
		  apmint_copy(y, yy)                          'y of ax + by = g
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_factorial(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // Returns the factorial of an apmint
		  //     The method is accelerated by maximizing the number of FFT multiplies.
		  //     Products are inserted into a cascade of apmint pairs to create near equal size numbers of FFT size
		  //     When a product in one tier takes place, the result is promoted to the next tier.  The product from
		  //     the final tier is multiplied by the running result 'output'.
		  //
		  //  At the end, the running factorial 'output' is multiplied by any remaining residuals left in the pairs.
		  //
		  var b1 As  apmint_module.apmint
		  Const pair_max = 12                                        ' number of pairs
		  var pairs(pair_max,1) As apmint_module.apmint              ' the pairs
		  var pair_fill(pair_max) As Int64                           ' used to track how much of a pair has been used (0=none, 1=first element, 2=all)
		  var threshold As Int64 = 1040                              ' size of apmint to transition to FFT multiplies
		  var pair_number As Int64                                   ' track movement between pairs
		  var ii As Int64
		  Static one as apmint_module.apmint = 1
		  
		  
		  
		  If input.Sign = -1 Then                                    ' input must be 0 or positive integer
		    var re As New RuntimeException
		    re.Message = "apmint_NegativeArgumentErr"
		    Raise re
		  End If
		  
		  apmint_copy(one, output)
		  
		  If input.Sign = 0 Or input = one Then return               ' 0! or 1! = 1
		  
		  For ii = 0 To pair_max                                     ' set pair default values to 1
		    pairs(ii,0) = one
		    pairs(ii,1) = one
		  Next
		  
		  apmint_copy(input, b1)
		  
		  While b1.Sign <> 0
		    pairs(0, pair_fill(0)) = pairs(0, pair_fill(0)) * b1
		    apmint_subtract(b1, one, b1)
		    If pairs(0, pair_fill(0)).digits.size > threshold Then    ' large enough for fft multiply?
		      pair_number = 0
		      While True
		        // progress through pairs
		        pair_fill(pair_number) = pair_fill(pair_number) + 1
		        If pair_fill(pair_number) < 2 Then Exit
		        // pair full, need to promote result to next pair
		        If pair_number = pair_max Then
		          output = output * apmint_fft_multiply(pairs(pair_number,0) , pairs(pair_number, 1))
		          pairs(pair_number,0) = one
		          pairs(pair_number,1) = one
		          pair_fill(pair_number) = 0
		          Exit
		        Else
		          pairs(pair_number +1, pair_fill(pair_number +1)) = apmint_fft_multiply(pairs(pair_number,0) , pairs(pair_number,1))
		          pairs(pair_number,0) = one
		          pairs(pair_number,1) = one
		          pair_fill(pair_number) = 0
		          pair_number = pair_number + 1
		        End If
		      Wend
		    End If
		  Wend
		  
		  // Multiply the running value 'nfact' by any pair residuals
		  // Only the first element of the pairs will have residuals, except for the first pair.
		  output = output * pairs(0,1)
		  For ii = 0 To pair_max
		    output = output * pairs(ii,0)
		  Next
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_bitrv2(power_of_two as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var j0, k0, j1, k1, l, m, i, j, k As Int64
		  var l1, l2 As Int64
		  var o1, o2, o3, o4 As Int64
		  var xr, xi, yr, yi As Double
		  
		  l = power_of_two \ 4
		  m = 2
		  While (m < l)
		    l = l \ 2
		    m = m * 2
		  Wend
		  If (m = l) Then
		    //j0 = 0
		    l1 = m - 1
		    For k0 = 0 To l1 Step 2
		      k = k0
		      l2 = j0 + k0 - 1
		      For j = j0 To l2 Step 2
		        o1 = j * 8
		        o2 = o1 + 8
		        o3 = k * 8
		        o4 = o3 + 8
		        xr = a.double(o1)
		        xi = a.double(o2)
		        yr = a.double(o3)
		        yi = a.double(o4)
		        a.double(o1) = yr
		        a.double(o2) = yi
		        a.double(o3) = xr
		        a.double(o4) = xi
		        j1 = j + m
		        k1 = k + 2 * m
		        o1 = j1 * 8
		        o2 = o1 + 8
		        o3 = k1 * 8
		        o4 = o3 + 8
		        xr = a.double(o1)
		        xi = a.double(o2)
		        yr = a.double(o3)
		        yi = a.double(o4)
		        a.double(o1) = yr
		        a.double(o2) = yi
		        a.double(o3) = xr
		        a.double(o4) = xi
		        j1 = j1 + m
		        k1 = k1 - m
		        o1 = j1 * 8
		        o2 = o1 + 8
		        o3 = k1 * 8
		        o4 = o3 + 8
		        xr = a.double(o1)
		        xi = a.double(o2)
		        yr = a.double(o3)
		        yi = a.double(o4)
		        a.double(o1) = yr
		        a.double(o2) = yi
		        a.double(o3) = xr
		        a.double(o4) = xi
		        j1 = j1 + m
		        k1 = k1 + (2 * m)
		        o1 = j1 * 8
		        o2 = o1 + 8
		        o3 = k1 * 8
		        o4 = o3 +  8
		        xr = a.double(o1)
		        xi = a.double(o2)
		        yr = a.double(o3)
		        yi = a.double(o4)
		        a.double(o1) = yr
		        a.double(o2) = yi
		        a.double(o3) = xr
		        a.double(o4) = xi
		        i = power_of_two \ 2
		        k = k Xor i
		        While i > k
		          i = i \ 2
		          k = k Xor i
		        Wend
		      Next
		      j1 = j0 + k0 + m
		      k1 = j1 + m
		      o1 = j1 * 8
		      o2 = o1 + 8
		      o3 = k1 * 8
		      o4 = o3 + 8
		      xr = a.double(o1)
		      xi = a.double(o2)
		      yr = a.double(o3)
		      yi = a.double(o4)
		      a.double(o1) = yr
		      a.double(o2) = yi
		      a.double(o3) = xr
		      a.double(o4) = xi
		      i = power_of_two \ 2
		      j0 = j0 Xor i
		      While i > j0
		        i = i \ 2
		        j0 = j0 Xor i
		      Wend
		    Next
		  Else
		    j0 = 0
		    l1 = m - 1
		    For k0 = 2 To l1 Step 2
		      i = power_of_two \ 2
		      j0 = j0 Xor i
		      While i > j0
		        i = i \ 2
		        j0 = j0 Xor i
		      Wend
		      k = k0
		      l2 = j0 + k0 -1
		      For j = j0 To l2 Step 2
		        o1 = j * 8
		        o2 = o1 + 8
		        o3 = k * 8
		        o4 = o3 + 8
		        xr = a.double(o1)
		        xi = a.double(o2)
		        yr = a.double(o3)
		        yi = a.double(o4)
		        a.double(o1) = yr
		        a.double(o2) = yi
		        a.double(o3) = xr
		        a.double(o4) = xi
		        j1 = j + m
		        k1 = k + m
		        o1 = j1 * 8
		        o2 = o1 + 8
		        o3 = k1 * 8
		        o4 = o3 + 8
		        xr = a.double(o1)
		        xi = a.double(o2)
		        yr = a.double(o3)
		        yi = a.double(o4)
		        a.double(o1) = yr
		        a.double(o2) = yi
		        a.double(o3) = xr
		        a.double(o4) = xi
		        i = power_of_two \ 2
		        k = k Xor i
		        While i > k
		          i = i \ 2
		          k = k Xor i
		        Wend
		      Next
		    Next
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_cft1st(power_of_two as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var j, kj, kr, l1 As Int64
		  var o0, o1, o2, o3, o4, o5, o6, o7 As Int64
		  var o8, o9, o10, o11, o12, o13, o14, o15 As Int64
		  var ew, wn4r, wk1r, wk1i, wk2r, wk2i, wk3r, wk3i As Double
		  var x0r, x0i, x1r, x1i, x2r, x2i, x3r, x3i As Double
		  
		  x0r = a.Double(0) + a.Double(16)
		  x0i = a.Double(8) + a.Double(24)
		  x1r = a.Double(0) - a.Double(16)
		  x1i = a.Double(8) - a.Double(24)
		  x2r = a.Double(32) + a.Double(48)
		  x2i = a.Double(40) + a.Double(56)
		  x3r = a.Double(32) - a.Double(48)
		  x3i = a.Double(40) - a.Double(56)
		  a.Double(0) = x0r + x2r
		  a.Double(8) = x0i + x2i
		  a.Double(32) = x0r - x2r
		  a.Double(40) = x0i - x2i
		  a.Double(16) = x1r - x3i
		  a.Double(24) = x1i + x3r
		  a.Double(48) = x1r + x3i
		  a.Double(56) = x1i - x3r
		  wn4r = 0.707106781186547524400844362104849039284835937688  //cos(pi_2*0.5)
		  x0r = a.Double(64) + a.Double(80)
		  x0i = a.Double(72) + a.Double(88)
		  x1r = a.Double(64) - a.Double(80)
		  x1i = a.Double(72) - a.Double(88)
		  x2r = a.Double(96) + a.Double(112)
		  x2i = a.Double(104) + a.Double(120)
		  x3r = a.Double(96) - a.Double(112)
		  x3i = a.Double(104) - a.Double(120)
		  a.Double(64) = x0r + x2r
		  a.Double(72) = x0i + x2i
		  a.Double(96) = x2i - x0i
		  a.Double(104) = x0r - x2r
		  x0r = x1r - x3i
		  x0i = x1i + x3r
		  a.Double(80) = wn4r * (x0r - x0i)
		  a.Double(88) = wn4r * (x0r + x0i)
		  x0r = x3i + x1r
		  x0i = x3r - x1i
		  a.Double(112) = wn4r * (x0i - x0r)
		  a.Double(120) = wn4r * (x0i + x0r)
		  ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		  //kr = 0
		  l1 = power_of_two - 1
		  For j = 16 To l1 Step 16
		    kj = power_of_two \ 4
		    kr = kr Xor kj
		    While (kj > kr)
		      kj = kj \ 2
		      kr = kr Xor kj
		    Wend 
		    wk1r = cos(ew * kr)
		    wk1i = sin(ew * kr)
		    wk2r = 1 - 2 * wk1i * wk1i
		    wk2i = 2 * wk1i * wk1r
		    wk3r = wk1r - 2 * wk2i * wk1i
		    wk3i = 2 * wk2i * wk1r - wk1i
		    o0 = j * 8
		    o1 = o0 + 8
		    o2 = o1 + 8
		    o3 = o2 + 8
		    o4 = o3 + 8
		    o5 = o4 + 8
		    o6 = o5 + 8
		    o7 = o6 + 8
		    x0r = a.Double(o0) + a.Double(o2)
		    x0i = a.Double(o1) + a.Double(o3)
		    x1r = a.Double(o0) - a.Double(o2)
		    x1i = a.Double(o1) - a.Double(o3)
		    x2r = a.Double(o4) + a.Double(o6)
		    x2i = a.Double(o5) + a.Double(o7)
		    x3r = a.Double(o4) - a.Double(o6)
		    x3i = a.Double(o5) - a.Double(o7)
		    a.Double(o0) = x0r + x2r
		    a.Double(o1) = x0i + x2i
		    x0r = x0r  - x2r
		    x0i = x0i - x2i
		    a.Double(o4) = wk2r * x0r - wk2i * x0i
		    a.Double(o5) = wk2r * x0i + wk2i * x0r
		    x0r = x1r - x3i
		    x0i = x1i + x3r
		    a.Double(o2) = wk1r * x0r - wk1i * x0i
		    a.Double(o3) = wk1r * x0i + wk1i * x0r
		    x0r = x1r + x3i
		    x0i = x1i - x3r
		    a.Double(o6) = wk3r * x0r - wk3i * x0i
		    a.Double(o7) = wk3r * x0i + wk3i * x0r
		    x0r = wn4r * (wk1r - wk1i)
		    wk1i = wn4r * (wk1r + wk1i)
		    wk1r = x0r
		    wk3r = wk1r - 2 * wk2r * wk1i
		    wk3i = 2 * wk2r * wk1r - wk1i
		    o8 = (j + 8) * 8
		    o9 = o8 + 8
		    o10 = o9 + 8
		    o11 = o10 + 8
		    o12 = o11 + 8
		    o13 = o12 + 8
		    o14 = o13 + 8
		    o15 = o14 + 8
		    x0r = a.Double(o8) + a.Double(o10)
		    x0i = a.Double(o9) + a.Double(o11)
		    x1r = a.Double(o8) - a.Double(o10)
		    x1i = a.Double(o9) - a.Double(o11)
		    x2r = a.Double(o12) + a.Double(o14)
		    x2i = a.Double(o13) + a.Double(o15)
		    x3r = a.Double(o12) - a.Double(o14)
		    x3i = a.Double(o13) - a.Double(o15)
		    a.Double(o8) = x0r + x2r
		    a.Double(o9) = x0i + x2i
		    x0r = x0r - x2r
		    x0i = x0i - x2i
		    a.Double(o12) = -wk2i * x0r - wk2r * x0i
		    a.Double(o13) = -wk2i * x0i + wk2r * x0r
		    x0r = x1r - x3i
		    x0i = x1i + x3r
		    a.Double(o10) = wk1r * x0r - wk1i * x0i
		    a.Double(o11) = wk1r * x0i + wk1i * x0r
		    x0r = x1r + x3i
		    x0i = x1i - x3r
		    a.Double(o14) = wk3r * x0r - wk3i * x0i
		    a.Double(o15) = wk3r * x0i + wk3i * x0r
		  Next
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_cftbsub(power_of_two as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var j, j1, j2, j3, l As Int64
		  var o1, o2, o3, o4, o5, o6, o7, o8 As Int64
		  var x0r, x0i, x1r, x1i, x2r, x2i, x3r, x3i As Double
		  var l1 As Int64
		  
		  l = 2
		  If (power_of_two > 8) Then
		    apmint_fft_cft1st(power_of_two, a)
		    l = 8
		    While l * 4 < power_of_two
		      apmint_fft_cftmdl(power_of_two, l, a)
		      l = l * 4
		    Wend
		  End If
		  If l * 4 = power_of_two Then
		    l1 = l - 1
		    For j = 0 To l1 Step 2
		      j1 = j + l
		      j2 = j1 + l
		      j3 = j2 + l
		      o1 = j * 8
		      o2 = j1 * 8
		      o3 = j2 * 8
		      o4 = j3 * 8
		      o5 = o1 + 8
		      o6 = o2 + 8
		      o7 = o3 + 8
		      o8 = o4 + 8
		      x0r = a.Double(o1) + a.Double(o2)
		      x0i = -a.Double(o5) - a.Double(o6)
		      x1r = a.Double(o1) - a.Double(o2)
		      x1i = -a.Double(o5) + a.Double(o6)
		      x2r = a.Double(o3) + a.Double(o4)
		      x2i = a.Double(o7) + a.Double(o8)
		      x3r = a.Double(o3) - a.Double(o4)
		      x3i = a.Double(o7) - a.Double(o8)
		      a.Double(o1) = x0r + x2r
		      a.Double(o5) = x0i - x2i
		      a.Double(o3) = x0r - x2r
		      a.Double(o7) = x0i + x2i
		      a.Double(o2)= x1r - x3i
		      a.Double(o6) = x1i - x3r
		      a.Double(o4) = x1r + x3i
		      a.Double(o8) = x1i + x3r
		    Next
		  Else
		    l1 = l - 1
		    For j = 0 To l1 Step 2
		      j1 = j + l
		      o1 = j * 8
		      o2 = j1 * 8
		      o3 = o1 + 8
		      o4 = o2 +  8
		      x0r = a.Double(o1) - a.Double(o2)
		      x0i = -a.Double(o3) + a.Double(o4)
		      a.Double(o1) = a.Double(o1) + a.Double(o2)
		      a.Double(o3) = -a.Double(o3) - a.Double(o4)
		      a.Double(o2) = x0r
		      a.Double(o4) = x0i
		    Next
		  End If
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_cftfsub(power_of_two as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var j,  j1, j2, j3, l , l1 As Int64
		  var o1, o2, o3, o4, o5, o6, o7, o8 As Int64
		  var x0r, x0i, x1r, x1i, x2r, x2i, x3r, x3i As Double
		  
		  l = 2
		  If (power_of_two > 8) Then
		    apmint_fft_cft1st(power_of_two, a)
		    l = 8
		    While l * 4 < power_of_two
		      apmint_fft_cftmdl(power_of_two, l, a)
		      l = l * 4
		    Wend
		  End If
		  If l * 4 = power_of_two Then
		    l1 = l - 1
		    For j = 0 To l1 Step 2
		      j1 = j + l
		      j2 = j1 + l
		      j3 = j2 + l
		      o1 = j * 8
		      o2 = j1 * 8
		      o3 = j2 * 8
		      o4 = j3 * 8
		      o5 = o1 + 8
		      o6 = o2 + 8
		      o7 = o3 + 8
		      o8 = o4 + 8
		      x0r = a.Double(o1) + a.Double(o2)
		      x0i = a.Double(o5) + a.Double(o6)
		      x1r = a.Double(o1) - a.Double(o2)
		      x1i = a.Double(o5) - a.Double(o6)
		      x2r = a.Double(o3) + a.Double(o4)
		      x2i = a.Double(o7) + a.Double(o8)
		      x3r = a.Double(o3) - a.Double(o4)
		      x3i = a.Double(o7) - a.Double(o8)
		      a.Double(o1) = x0r + x2r
		      a.Double(o5) = x0i + x2i
		      a.Double(o3) = x0r - x2r
		      a.Double(o7) = x0i - x2i
		      a.Double(o2) = x1r - x3i
		      a.Double(o6) = x1i + x3r
		      a.Double(o4) = x1r + x3i
		      a.Double(o8) = x1i - x3r
		    Next
		  Else
		    l1 = l - 1
		    For j = 0 To  l1 Step 2
		      j1 = j + l
		      o1 = j * 8
		      o2 = j1 * 8
		      o3 = o1 + 8
		      o4 = o2 + 8
		      x0r = a.Double(o1) - a.Double(o2)
		      x0i = a.Double(o3) - a.Double(o4)
		      a.Double(o1) = a.Double(o1) + a.Double(o2)
		      a.Double(o3) = a.Double(o3) + a.Double(o4)
		      a.Double(o2) = x0r
		      a.Double(o4) = x0i
		    Next
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_cftmdl(power_of_two as int64, l as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var j, j1, j2, j3, k, kj, kr, m, m2, l1, l2 As Int64
		  var o1, o2, o3, o4, o5, o6, o7, o8 As Int64
		  var ew, wn4r, wk1r, wk1i, wk2r, wk2i, wk3r, wk3i As Double
		  var x0r, x0i, x1r, x1i, x2r, x2i, x3r, x3i As Double
		  
		  m = l * 4
		  l1 = l - 1
		  For j = 0 To  l1 Step 2
		    j1 = j + l
		    j2 = j1 + l
		    j3 = j2 + l
		    o1 = j * 8
		    o2 = j1 * 8
		    o3 = j2 * 8
		    o4 = j3 * 8
		    o5 = o1 + 8
		    o6 = o2 + 8
		    o7 = o3 + 8
		    o8 = o4 + 8
		    x0r = a.Double(o1) + a.Double(o2)
		    x0i = a.Double(o5) + a.Double(o6)
		    x1r = a.Double(o1) - a.Double(o2)
		    x1i = a.Double(o5) - a.Double(o6)
		    x2r = a.Double(o3) + a.Double(o4)
		    x2i = a.Double(o7) + a.Double(o8)
		    x3r = a.Double(o3) - a.Double(o4)
		    x3i = a.Double(o7) - a.Double(o8)
		    a.Double(o1) = x0r + x2r
		    a.Double(o5) = x0i + x2i
		    a.Double(o3) = x0r - x2r
		    a.Double(o7) = x0i - x2i
		    a.Double(o2) = x1r - x3i
		    a.Double(o6) = x1i + x3r
		    a.Double(o4) = x1r + x3i
		    a.Double(o8) = x1i - x3r
		  Next
		  wn4r = 0.707106781186547524400844362104849039284835937688
		  l1 = l + m - 1
		  For j = m To l1 Step 2
		    j1 = j + l
		    j2 = j1 + l
		    j3 = j2 + l
		    o1 = j * 8
		    o2 = j1 * 8
		    o3 = j2 * 8
		    o4 = j3 * 8
		    o5 = o1 + 8
		    o6 = o2 + 8
		    o7 = o3 + 8
		    o8 = o4 + 8
		    x0r = a.Double(o1) + a.Double(o2)
		    x0i = a.Double(o5) + a.Double(o6)
		    x1r = a.Double(o1) - a.Double(o2)
		    x1i = a.Double(o5) - a.Double(o6)
		    x2r = a.Double(o3) + a.Double(o4)
		    x2i = a.Double(o7) + a.Double(o8)
		    x3r = a.Double(o3) - a.Double(o4)
		    x3i = a.Double(o7) - a.Double(o8)
		    a.Double(o1) = x0r + x2r
		    a.Double(o5) = x0i + x2i
		    a.Double(o3) = x2i - x0i
		    a.Double(o7) = x0r - x2r
		    x0r = x1r - x3i
		    x0i = x1i + x3r
		    a.Double(o2) = wn4r * (x0r - x0i)
		    a.Double(o6) = wn4r * (x0r + x0i)
		    x0r = x3i + x1r
		    x0i = x3r - x1i
		    a.Double(o4) = wn4r * (x0i - x0r)
		    a.Double(o8) = wn4r * (x0i + x0r)
		  Next
		  ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		  kr = 0
		  m2 = 2 * m
		  l1 = power_of_two - 1
		  For k = m2 To l1 Step m2
		    kj = power_of_two \ 4
		    kr = kr Xor kj
		    While kj > kr
		      kj = kj \ 2
		      kr = kr Xor kj
		    Wend
		    wk1r = cos(ew * kr)
		    wk1i = sin(ew * kr)
		    wk2r = 1 - 2 * wk1i * wk1i
		    wk2i = 2 * wk1i * wk1r
		    wk3r = wk1r - 2 * wk2i * wk1i
		    wk3i = 2 * wk2i * wk1r - wk1i
		    l2 = l + k - 1
		    For j = k To l2 Step 2
		      j1 = j + l
		      j2 = j1 + l
		      j3 = j2 + l
		      o1 = j * 8
		      o2 = j1 * 8
		      o3 = j2 * 8
		      o4 = j3 * 8
		      o5 = o1 + 8
		      o6 = o2 + 8
		      o7 = o3 + 8
		      o8 = o4 + 8
		      x0r = a.Double(o1) + a.Double(o2)
		      x0i = a.Double(o5) + a.Double(o6)
		      x1r = a.Double(o1) - a.Double(o2)
		      x1i = a.Double(o5) - a.Double(o6)
		      x2r = a.Double(o3) + a.Double(o4)
		      x2i = a.Double(o7) + a.Double(o8)
		      x3r = a.Double(o3) - a.Double(o4)
		      x3i = a.Double(o7) - a.Double(o8)
		      a.Double(o1) = x0r + x2r
		      a.Double(o5) = x0i + x2i
		      x0r = x0r - x2r
		      x0i = x0i - x2i
		      a.Double(o3) = wk2r * x0r - wk2i * x0i
		      a.Double(o7) = wk2r * x0i + wk2i * x0r
		      x0r = x1r - x3i
		      x0i = x1i + x3r
		      a.Double(o2) = wk1r * x0r - wk1i * x0i
		      a.Double(o6) = wk1r * x0i + wk1i * x0r
		      x0r = x1r + x3i
		      x0i = x1i - x3r
		      a.Double(o4) = wk3r * x0r - wk3i * x0i
		      a.Double(o8) = wk3r * x0i + wk3i * x0r
		    Next
		    x0r = wn4r * (wk1r - wk1i)
		    wk1i = wn4r * (wk1r + wk1i)
		    wk1r = x0r
		    wk3r = wk1r - 2 * wk2r * wk1i
		    wk3i = 2 * wk2r * wk1r - wk1i
		    l2 = l + (k + m) - 1
		    For j = k + m To l2 Step 2
		      j1 = j + l
		      j2 = j1 + l
		      j3 = j2 + l
		      o1 = j * 8
		      o2 = j1 * 8
		      o3 = j2 * 8
		      o4 = j3 * 8
		      o5 = o1 + 8
		      o6 = o2 + 8
		      o7 = o3 + 8
		      o8 = o4 + 8
		      x0r = a.Double(o1) + a.Double(o2)
		      x0i = a.Double(o5) + a.Double(o6)
		      x1r = a.Double(o1) - a.Double(o2)
		      x1i = a.Double(o5) - a.Double(o6)
		      x2r = a.Double(o3) + a.Double(o4)
		      x2i = a.Double(o7) + a.Double(o8)
		      x3r = a.Double(o3) - a.Double(o4)
		      x3i = a.Double(o7) - a.Double(o8)
		      a.Double(o1) = x0r + x2r
		      a.Double(o5) = x0i + x2i
		      x0r = x0r - x2r
		      x0i = x0i - x2i
		      a.Double(o3) = -wk2i * x0r - wk2r * x0i
		      a.Double(o7) = -wk2i * x0i + wk2r * x0r
		      x0r = x1r - x3i
		      x0i = x1i + x3r
		      a.Double(o2) = wk1r * x0r - wk1i * x0i
		      a.Double(o6) = wk1r * x0i + wk1i * x0r
		      x0r = x1r + x3i
		      x0i = x1i - x3r
		      a.Double(o4) = wk3r * x0r - wk3i * x0i
		      a.Double(o8) = wk3r * x0i + wk3i * x0r
		    Next
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_fft_multiply(num1 as apmint_module.apmint, num2 as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // multiply two apmints using fft multiply algorithm
		  
		  If num1.Sign * num2.Sign = 0 Then Return New apmint_module.apmint
		  
		  // will be breaking down apmints into 8 8-bit words; determine how many per input and max
		  var a, b As apmint_module.apmint
		  var a_words, b_words, max_words As Int64
		  a = num1
		  b = num2
		  var a_ptr As ptr = a.digits
		  var b_ptr As ptr = b.digits
		  a_words = a.used
		  b_words = b.used
		  max_words = a_words
		  If b_words > max_words Then max_words = b_words
		  
		  // size fft arrays; array size must be a power of two with embedded input <=  half of that
		  var fft_array_size As Int64 = 2
		  While fft_array_size < max_words
		    fft_array_size = fft_array_size * 2
		  Wend
		  fft_array_size = fft_array_size * 2
		  
		  var mb_a As New memoryblock(fft_array_size * 8)
		  var mb_b As New memoryblock(fft_array_size * 8)
		  var mb_a_ptr As ptr = mb_a
		  var mb_b_ptr As ptr = mb_b
		  
		  // load first number into a fft array from most significant to least significant word
		  var p1, p2, p3, p4 As Int64
		  var utemp As UInt64
		  p1 = a.used - 1
		  p2 = 0
		  While p1 >= 0
		    mb_a_ptr.Double(p2) = a_ptr.UInt8(p1)
		    p1 = p1 - 1
		    p2 = p2 + 8
		  Wend
		  
		  // load second number into a fft array
		  p1 = b.used - 1
		  p2 = 0
		  While p1 >= 0
		    mb_b_ptr.Double(p2) = b_ptr.UInt8(p1)
		    p1 = p1 - 1
		    p2 = p2 + 8
		  Wend
		  
		  apmint_fft_rdft(fft_array_size, 1, mb_a)          // forward transform on first array
		  apmint_fft_rdft(fft_array_size, 1, mb_b)          // forward transform on second array
		  
		  // convolution
		  var xr, xi, yr, yi As Double
		  mb_b_ptr.Double(0) = mb_b_ptr.Double(0) * mb_a_ptr.Double(0)
		  mb_b_ptr.Double(8) = mb_b_ptr.Double(8) * mb_a_ptr.Double(8)
		  p4 = fft_array_size - 2
		  For p1 = 2 To p4 Step 2
		    p2 = p1 * 8
		    p3 = p2 + 8
		    xr = mb_b_ptr.Double(p2)
		    xi = mb_b_ptr.Double(p3)
		    yr = mb_a_ptr.Double(p2)
		    yi = mb_a_ptr.Double(p3)
		    mb_b_ptr.Double(p2) = xr * yr - xi * yi
		    mb_b_ptr.Double(p3) = xr * yi + xi * yr
		  Next
		  
		  // inverse transform
		  apmint_fft_rdft(fft_array_size, -1, mb_b)
		  
		  //remove base-256 carries
		  p1 = fft_array_size
		  p2 = fft_array_size * 8 - 8
		  var carry, nnr, dtemp As Double
		  carry = 0.0
		  nnr = 2.0 / fft_array_size
		  While p1 > 0
		    dtemp = mb_b_ptr.Double(p2) * nnr + carry + 0.5
		    utemp = CType(dtemp / 256.0, UInt64)
		    carry = CType(utemp, Double)
		    mb_b_ptr.UInt64(p2) = CType(dtemp - carry * 256.0, UInt64)
		    p1 = p1 - 1
		    p2 = p2 - 8
		  Wend
		  
		  var ucarry As UInt64 = CType(carry, UInt64)
		  
		  // convert result back to a bigint
		  var result As apmint_module.apmint = 0
		  result.used = a_words + b_words
		  result.digits = New memoryblock(result.used)
		  var rptr As ptr = result.digits
		  p2 = (a_words + b_words - 2) * 8
		  While p2 >= 0
		    rptr.UInt8(p1) = mb_b_ptr.UInt64(p2)
		    p2 = p2 - 8
		    p1 = p1 + 1
		  Wend
		  If ucarry <> 0 Then rptr.UInt8(p1) = ucarry
		  
		  result.Sign = a.Sign * b.Sign
		  result.normalize
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_rdft(power_of_two as int64, type as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  // performs a forward or reverse transform on array 'a'
		  // power_of_two is size of array
		  // type is positive for forward transform, negative for reverse transform
		  
		  var x As Double
		  
		  If (type >= 0) Then
		    If (power_of_two > 4) Then
		      apmint_fft_bitrv2(power_of_two, a)
		      apmint_fft_cftfsub(power_of_two, a)
		      apmint_fft_rftfsub(power_of_two, a)
		    Elseif (power_of_two = 4) Then
		      apmint_fft_cftfsub(power_of_two, a)
		    End If
		    x = a.Double(0) - a.Double(8)
		    a.Double(0) = a.Double(0) + a.Double(8)
		    a.Double(8) = x
		  Else
		    a.Double(8) = 0.5 * (a.Double(0) - a.Double(8))
		    a.Double(0) = a.Double(0) - a.Double(8)
		    If (power_of_two > 4) Then
		      apmint_fft_rftbsub(power_of_two, a)
		      apmint_fft_bitrv2(power_of_two, a)
		      apmint_fft_cftbsub(power_of_two, a)
		    Elseif (power_of_two = 4) Then
		      apmint_fft_cftfsub(power_of_two, a)
		    End If
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_rdft_all(power_of_two as int64, type as int64, a as Ptr)
		  // this is a flattened version of the simple fft routine where all functions are integrated into one method
		  
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  
		  
		  // performs a forward or reverse transform on array 'a'
		  // power_of_two is size of array
		  // type is positive for forward transform, negative for reverse transform
		  
		  var k0, k1, m, m2, k, kj, kr As Int64
		  var i, i0, j,  j0, j1, j2, j3, l , l1, l2 As Int64
		  var o0, o1, o2, o3, o4, o5, o6, o7, o8 As Int64
		  var o9, o10, o11, o12, o13, o14, o15 As Int64
		  var xr, xi, yr, yi, x0r, x0i, x1r, x1i, x2r, x2i, x3r, x3i As Double
		  var ec, w1r, w1i, wkr, wki, wdr, wdi, ss As Double
		  var ew, wn4r, wk1r, wk1i, wk2r, wk2i, wk3r, wk3i As Double
		  
		  If (type >= 0) Then
		    If (power_of_two > 4) Then
		      l = bitwise.ShiftRight(power_of_two, 2)
		      m = 2
		      While (m < l)
		        l = bitwise.ShiftRight(l, 1)
		        m = bitwise.ShiftLeft(m, 1)
		      Wend
		      If (m = l) Then
		        j0 = 0
		        l1 = m - 1
		        For k0 = 0 To l1 Step 2
		          k = k0
		          l2 = j0 + k0 - 1
		          For j = j0 To l2 Step 2
		            o1 = j * 8
		            o2 = o1 + 8
		            o3 = k * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j + m
		            k1 = k + 2 * m
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j1 + m
		            k1 = k1 - m
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j1 + m
		            k1 = k1 + (2 * m)
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 +  8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            i = bitwise.ShiftRight(power_of_two, 1)
		            k = k Xor i
		            While i > k
		              i = bitwise.ShiftRight(i, 1)
		              k = k Xor i
		            Wend
		          Next
		          j1 = j0 + k0 + m
		          k1 = j1 + m
		          o1 = j1 * 8
		          o2 = o1 + 8
		          o3 = k1 * 8
		          o4 = o3 + 8
		          xr = a.double(o1)
		          xi = a.double(o2)
		          yr = a.double(o3)
		          yi = a.double(o4)
		          a.double(o1) = yr
		          a.double(o2) = yi
		          a.double(o3) = xr
		          a.double(o4) = xi
		          i = bitwise.ShiftRight(power_of_two, 1)
		          j0 = j0 Xor i
		          While i > j0
		            i = bitwise.ShiftRight(i, 1)
		            j0 = j0 Xor i
		          Wend
		        Next
		      Else
		        j0 = 0
		        l1 = m - 1
		        For k0 = 2 To l1 Step 2
		          i = bitwise.ShiftRight(power_of_two, 1)
		          j0 = j0 Xor i
		          While i > j0
		            i = bitwise.ShiftRight(i, 1)
		            j0 = j0 Xor i
		          Wend
		          k = k0
		          l2 = j0 + k0 -1
		          For j = j0 To l2 Step 2
		            o1 = j * 8
		            o2 = o1 + 8
		            o3 = k * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j + m
		            k1 = k + m
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            i = bitwise.ShiftRight(power_of_two, 1)
		            k = k Xor i
		            While i > k
		              i = bitwise.ShiftRight(i, 1)
		              k = k Xor i
		            Wend
		          Next
		        Next
		      End If
		      l = 2
		      If (power_of_two > 8) Then
		        x0r = a.Double(0) + a.Double(16)
		        x0i = a.Double(8) + a.Double(24)
		        x1r = a.Double(0) - a.Double(16)
		        x1i = a.Double(8) - a.Double(24)
		        x2r = a.Double(32) + a.Double(48)
		        x2i = a.Double(40) + a.Double(56)
		        x3r = a.Double(32) - a.Double(48)
		        x3i = a.Double(40) - a.Double(56)
		        a.Double(0) = x0r + x2r
		        a.Double(8) = x0i + x2i
		        a.Double(32) = x0r - x2r
		        a.Double(40) = x0i - x2i
		        a.Double(16) = x1r - x3i
		        a.Double(24) = x1i + x3r
		        a.Double(48) = x1r + x3i
		        a.Double(56) = x1i - x3r
		        wn4r = 0.707106781186547524400844362104849039284835937688  //cos(pi_2*0.5)
		        x0r = a.Double(64) + a.Double(80)
		        x0i = a.Double(72) + a.Double(88)
		        x1r = a.Double(64) - a.Double(80)
		        x1i = a.Double(72) - a.Double(88)
		        x2r = a.Double(96) + a.Double(112)
		        x2i = a.Double(104) + a.Double(120)
		        x3r = a.Double(96) - a.Double(112)
		        x3i = a.Double(104) - a.Double(120)
		        a.Double(64) = x0r + x2r
		        a.Double(72) = x0i + x2i
		        a.Double(96) = x2i - x0i
		        a.Double(104) = x0r - x2r
		        x0r = x1r - x3i
		        x0i = x1i + x3r
		        a.Double(80) = wn4r * (x0r - x0i)
		        a.Double(88) = wn4r * (x0r + x0i)
		        x0r = x3i + x1r
		        x0i = x3r - x1i
		        a.Double(112) = wn4r * (x0i - x0r)
		        a.Double(120) = wn4r * (x0i + x0r)
		        ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		        kr = 0
		        l1 = power_of_two - 1
		        For j = 16 To l1 Step 16
		          kj = bitwise.ShiftRight(power_of_two, 2)
		          kr = kr Xor kj
		          While (kj > kr)
		            kj = bitwise.ShiftRight(kj, 1)
		            kr = kr Xor kj
		          Wend
		          wk1r = cos(ew * kr)
		          wk1i = sin(ew * kr)
		          wk2r = 1 - 2 * wk1i * wk1i
		          wk2i = 2 * wk1i * wk1r
		          wk3r = wk1r - 2 * wk2i * wk1i
		          wk3i = 2 * wk2i * wk1r - wk1i
		          o0 = j * 8
		          o1 = o0 + 8
		          o2 = o1 + 8
		          o3 = o2 + 8
		          o4 = o3 + 8
		          o5 = o4 + 8
		          o6 = o5 + 8
		          o7 = o6 + 8
		          x0r = a.Double(o0) + a.Double(o2)
		          x0i = a.Double(o1) + a.Double(o3)
		          x1r = a.Double(o0) - a.Double(o2)
		          x1i = a.Double(o1) - a.Double(o3)
		          x2r = a.Double(o4) + a.Double(o6)
		          x2i = a.Double(o5) + a.Double(o7)
		          x3r = a.Double(o4) - a.Double(o6)
		          x3i = a.Double(o5) - a.Double(o7)
		          a.Double(o0) = x0r + x2r
		          a.Double(o1) = x0i + x2i
		          x0r = x0r  - x2r
		          x0i = x0i - x2i
		          a.Double(o4) = wk2r * x0r - wk2i * x0i
		          a.Double(o5) = wk2r * x0i + wk2i * x0r
		          x0r = x1r - x3i
		          x0i = x1i + x3r
		          a.Double(o2) = wk1r * x0r - wk1i * x0i
		          a.Double(o3) = wk1r * x0i + wk1i * x0r
		          x0r = x1r + x3i
		          x0i = x1i - x3r
		          a.Double(o6) = wk3r * x0r - wk3i * x0i
		          a.Double(o7) = wk3r * x0i + wk3i * x0r
		          x0r = wn4r * (wk1r - wk1i)
		          wk1i = wn4r * (wk1r + wk1i)
		          wk1r = x0r
		          wk3r = wk1r - 2 * wk2r * wk1i
		          wk3i = 2 * wk2r * wk1r - wk1i
		          o8 = (j + 8) * 8
		          o9 = o8 + 8
		          o10 = o9 + 8
		          o11 = o10 + 8
		          o12 = o11 + 8
		          o13 = o12 + 8
		          o14 = o13 + 8
		          o15 = o14 + 8
		          x0r = a.Double(o8) + a.Double(o10)
		          x0i = a.Double(o9) + a.Double(o11)
		          x1r = a.Double(o8) - a.Double(o10)
		          x1i = a.Double(o9) - a.Double(o11)
		          x2r = a.Double(o12) + a.Double(o14)
		          x2i = a.Double(o13) + a.Double(o15)
		          x3r = a.Double(o12) - a.Double(o14)
		          x3i = a.Double(o13) - a.Double(o15)
		          a.Double(o8) = x0r + x2r
		          a.Double(o9) = x0i + x2i
		          x0r = x0r - x2r
		          x0i = x0i - x2i
		          a.Double(o12) = -wk2i * x0r - wk2r * x0i
		          a.Double(o13) = -wk2i * x0i + wk2r * x0r
		          x0r = x1r - x3i
		          x0i = x1i + x3r
		          a.Double(o10) = wk1r * x0r - wk1i * x0i
		          a.Double(o11) = wk1r * x0i + wk1i * x0r
		          x0r = x1r + x3i
		          x0i = x1i - x3r
		          a.Double(o14) = wk3r * x0r - wk3i * x0i
		          a.Double(o15) = wk3r * x0i + wk3i * x0r
		        Next
		        l = 8
		        While ( bitwise.ShiftLeft(l, 2) < power_of_two)
		          m = bitwise.ShiftLeft(l, 2)
		          l1 = l - 1
		          For j = 0 To  l1 Step 2
		            j1 = j + l
		            j2 = j1 + l
		            j3 = j2 + l
		            o1 = j * 8
		            o2 = j1 * 8
		            o3 = j2 * 8
		            o4 = j3 * 8
		            o5 = o1 + 8
		            o6 = o2 + 8
		            o7 = o3 + 8
		            o8 = o4 + 8
		            x0r = a.Double(o1) + a.Double(o2)
		            x0i = a.Double(o5) + a.Double(o6)
		            x1r = a.Double(o1) - a.Double(o2)
		            x1i = a.Double(o5) - a.Double(o6)
		            x2r = a.Double(o3) + a.Double(o4)
		            x2i = a.Double(o7) + a.Double(o8)
		            x3r = a.Double(o3) - a.Double(o4)
		            x3i = a.Double(o7) - a.Double(o8)
		            a.Double(o1) = x0r + x2r
		            a.Double(o5) = x0i + x2i
		            a.Double(o3) = x0r - x2r
		            a.Double(o7) = x0i - x2i
		            a.Double(o2) = x1r - x3i
		            a.Double(o6) = x1i + x3r
		            a.Double(o4) = x1r + x3i
		            a.Double(o8) = x1i - x3r
		          Next
		          wn4r = 0.707106781186547524400844362104849039284835937688
		          l1 = l + m - 1
		          For j = m To l1 Step 2
		            j1 = j + l
		            j2 = j1 + l
		            j3 = j2 + l
		            o1 = j * 8
		            o2 = j1 * 8
		            o3 = j2 * 8
		            o4 = j3 * 8
		            o5 = o1 + 8
		            o6 = o2 + 8
		            o7 = o3 + 8
		            o8 = o4 + 8
		            x0r = a.Double(o1) + a.Double(o2)
		            x0i = a.Double(o5) + a.Double(o6)
		            x1r = a.Double(o1) - a.Double(o2)
		            x1i = a.Double(o5) - a.Double(o6)
		            x2r = a.Double(o3) + a.Double(o4)
		            x2i = a.Double(o7) + a.Double(o8)
		            x3r = a.Double(o3) - a.Double(o4)
		            x3i = a.Double(o7) - a.Double(o8)
		            a.Double(o1) = x0r + x2r
		            a.Double(o5) = x0i + x2i
		            a.Double(o3) = x2i - x0i
		            a.Double(o7) = x0r - x2r
		            x0r = x1r - x3i
		            x0i = x1i + x3r
		            a.Double(o2) = wn4r * (x0r - x0i)
		            a.Double(o6) = wn4r * (x0r + x0i)
		            x0r = x3i + x1r
		            x0i = x3r - x1i
		            a.Double(o4) = wn4r * (x0i - x0r)
		            a.Double(o8) = wn4r * (x0i + x0r)
		          Next
		          ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		          kr = 0
		          m2 = 2 * m
		          l1 = power_of_two - 1
		          For k = m2 To l1 Step m2
		            kj = bitwise.ShiftRight(power_of_two, 2)
		            kr = kr Xor kj
		            While kj > kr
		              kj = bitwise.ShiftRight(kj, 1)
		              kr = kr Xor kj
		            Wend
		            wk1r = cos(ew * kr)
		            wk1i = sin(ew * kr)
		            wk2r = 1 - 2 * wk1i * wk1i
		            wk2i = 2 * wk1i * wk1r
		            wk3r = wk1r - 2 * wk2i * wk1i
		            wk3i = 2 * wk2i * wk1r - wk1i
		            l2 = l + k - 1
		            For j = k To l2 Step 2
		              j1 = j + l
		              j2 = j1 + l
		              j3 = j2 + l
		              o1 = j * 8
		              o2 = j1 * 8
		              o3 = j2 * 8
		              o4 = j3 * 8
		              o5 = o1 + 8
		              o6 = o2 + 8
		              o7 = o3 + 8
		              o8 = o4 + 8
		              x0r = a.Double(o1) + a.Double(o2)
		              x0i = a.Double(o5) + a.Double(o6)
		              x1r = a.Double(o1) - a.Double(o2)
		              x1i = a.Double(o5) - a.Double(o6)
		              x2r = a.Double(o3) + a.Double(o4)
		              x2i = a.Double(o7) + a.Double(o8)
		              x3r = a.Double(o3) - a.Double(o4)
		              x3i = a.Double(o7) - a.Double(o8)
		              a.Double(o1) = x0r + x2r
		              a.Double(o5) = x0i + x2i
		              x0r = x0r - x2r
		              x0i = x0i - x2i
		              a.Double(o3) = wk2r * x0r - wk2i * x0i
		              a.Double(o7) = wk2r * x0i + wk2i * x0r
		              x0r = x1r - x3i
		              x0i = x1i + x3r
		              a.Double(o2) = wk1r * x0r - wk1i * x0i
		              a.Double(o6) = wk1r * x0i + wk1i * x0r
		              x0r = x1r + x3i
		              x0i = x1i - x3r
		              a.Double(o4) = wk3r * x0r - wk3i * x0i
		              a.Double(o8) = wk3r * x0i + wk3i * x0r
		            Next
		            x0r = wn4r * (wk1r - wk1i)
		            wk1i = wn4r * (wk1r + wk1i)
		            wk1r = x0r
		            wk3r = wk1r - 2 * wk2r * wk1i
		            wk3i = 2 * wk2r * wk1r - wk1i
		            l2 = l + (k + m) - 1
		            For j = k + m To l2 Step 2
		              j1 = j + l
		              j2 = j1 + l
		              j3 = j2 + l
		              o1 = j * 8
		              o2 = j1 * 8
		              o3 = j2 * 8
		              o4 = j3 * 8
		              o5 = o1 + 8
		              o6 = o2 + 8
		              o7 = o3 + 8
		              o8 = o4 + 8
		              x0r = a.Double(o1) + a.Double(o2)
		              x0i = a.Double(o5) + a.Double(o6)
		              x1r = a.Double(o1) - a.Double(o2)
		              x1i = a.Double(o5) - a.Double(o6)
		              x2r = a.Double(o3) + a.Double(o4)
		              x2i = a.Double(o7) + a.Double(o8)
		              x3r = a.Double(o3) - a.Double(o4)
		              x3i = a.Double(o7) - a.Double(o8)
		              a.Double(o1) = x0r + x2r
		              a.Double(o5) = x0i + x2i
		              x0r = x0r - x2r
		              x0i = x0i - x2i
		              a.Double(o3) = -wk2i * x0r - wk2r * x0i
		              a.Double(o7) = -wk2i * x0i + wk2r * x0r
		              x0r = x1r - x3i
		              x0i = x1i + x3r
		              a.Double(o2) = wk1r * x0r - wk1i * x0i
		              a.Double(o6) = wk1r * x0i + wk1i * x0r
		              x0r = x1r + x3i
		              x0i = x1i - x3r
		              a.Double(o4) = wk3r * x0r - wk3i * x0i
		              a.Double(o8) = wk3r * x0i + wk3i * x0r
		            Next
		          Next
		          l = bitwise.ShiftLeft(l, 2)
		        Wend
		      End If
		      If (bitwise.ShiftLeft(l, 2) = power_of_two) Then
		        l1 = l - 1
		        For j = 0 To l1 Step 2
		          j1 = j + l
		          j2 = j1 + l
		          j3 = j2 + l
		          o1 = j * 8
		          o2 = j1 * 8
		          o3 = j2 * 8
		          o4 = j3 * 8
		          o5 = o1 + 8
		          o6 = o2 + 8
		          o7 = o3 + 8
		          o8 = o4 + 8
		          x0r = a.Double(o1) + a.Double(o2)
		          x0i = a.Double(o5) + a.Double(o6)
		          x1r = a.Double(o1) - a.Double(o2)
		          x1i = a.Double(o5) - a.Double(o6)
		          x2r = a.Double(o3) + a.Double(o4)
		          x2i = a.Double(o7) + a.Double(o8)
		          x3r = a.Double(o3) - a.Double(o4)
		          x3i = a.Double(o7) - a.Double(o8)
		          a.Double(o1) = x0r + x2r
		          a.Double(o5) = x0i + x2i
		          a.Double(o3) = x0r - x2r
		          a.Double(o7) = x0i - x2i
		          a.Double(o2) = x1r - x3i
		          a.Double(o6) = x1i + x3r
		          a.Double(o4) = x1r + x3i
		          a.Double(o8) = x1i - x3r
		        Next
		      Else
		        l1 = l - 1
		        For j = 0 To  l1 Step 2
		          j1 = j + l
		          o1 = j * 8
		          o2 = j1 * 8
		          o3 = o1 + 8
		          o4 = o2 + 8
		          x0r = a.Double(o1) - a.Double(o2)
		          x0i = a.Double(o3) - a.Double(o4)
		          a.Double(o1) = a.Double(o1) + a.Double(o2)
		          a.Double(o3) = a.Double(o3) + a.Double(o4)
		          a.Double(o2) = x0r
		          a.Double(o4) = x0i
		        Next
		      End If
		      ec = 2. * 1.570796326794896619231321691639751442098584699687 / power_of_two
		      wdi = cos(ec)
		      wdr = sin(ec)
		      wdi = wdi * wdr
		      wdr = wdr * wdr
		      w1r = 1. - 2. * wdr
		      w1i = 2. * wdi
		      ss = 2. * w1i
		      i = bitwise.ShiftRight(power_of_two, 1)
		      While True
		        i0 = i - 256 //4 * 64
		        If (i0 < 4) Then i0 = 4
		        For j = i - 4 DownTo i0 Step 4
		          k = power_of_two - j
		          o1 = j * 8
		          o2 = k * 8
		          o3 = o1 + 8
		          o4 = o3 + 8
		          o5 = o4 + 8
		          o6 = o2 + 8
		          o7 = o2 - 8
		          o8 = o7 - 8
		          xr = a.Double(o4) - a.Double(o8)
		          xi = a.Double(o5) + a.Double(o7)
		          yr = wdr * xr - wdi * xi
		          yi = wdr * xi + wdi * xr
		          a.Double(o4) = a.Double(o4) - yr
		          a.Double(o5) = a.Double(o5) - yi
		          a.Double(o8) = a.Double(o8) + yr
		          a.Double(o7) =  a.Double(o7) - yi
		          wkr = wkr + (ss * wdi)
		          wki = wki  + (ss * (0.5 - wdr))
		          xr = a.Double(o1) - a.Double(o2)
		          xi = a.Double(o3) + a.Double(o6)
		          yr = wkr * xr - wki * xi
		          yi = wkr * xi + wki * xr
		          a.Double(o1) =  a.Double(o1) - yr
		          a.Double(o3) = a.Double(o3) - yi
		          a.Double(o2) = a.Double(o2) + yr
		          a.Double(o6) = a.Double(o6) - yi
		          wdr = wdr + (ss * wki)
		          wdi  = wdi + (ss * (0.5 - wkr))
		        Next
		        If (i0 = 4)  Then Exit
		        wkr = 0.5 * sin(ec * i0)
		        wki = 0.5 * cos(ec * i0)
		        wdr = 0.5 - (wkr * w1r - wki * w1i)
		        wdi = wkr * w1i + wki * w1r
		        wkr = 0.5 - wkr
		        i = i0
		      Wend
		      o1 = (power_of_two - 1) * 8
		      o2 = o1 -  8
		      xr = a.Double(16) - a.Double(o2)
		      xi = a.Double(24) + a.Double(o1)
		      yr = wdr * xr - wdi * xi
		      yi = wdr * xi + wdi * xr
		      a.Double(16) =  a.Double(16) - yr
		      a.Double(24) =  a.Double(24) - yi
		      a.Double(o2) =  a.Double(o2) + yr
		      a.Double(o1) =  a.Double(o1) - yi
		    Elseif (power_of_two = 4) Then
		      ec = 2. * 1.570796326794896619231321691639751442098584699687 / power_of_two
		      wdi = cos(ec)
		      wdr = sin(ec)
		      wdi = wdi * wdr
		      wdr = wdr * wdr
		      w1r = 1. - 2. * wdr
		      w1i = 2. * wdi
		      ss = 2. * w1i
		      i = bitwise.ShiftRight(power_of_two, 1)
		      While True
		        i0 = i - 256
		        If (i0 < 4) Then i0 = 4
		        For j = i - 4 DownTo i0 Step 4
		          k = power_of_two - j
		          o1 = j * 8
		          o2 = k * 8
		          o3 = o1 + 8
		          o4 = o3 + 8
		          o5 = o4 + 8
		          o6 = o2 + 8
		          o7 = o2 - 8
		          o8 = o7 - 8
		          xr = a.Double(o4) - a.Double(o8)
		          xi = a.Double(o5) + a.Double(o7)
		          yr = wdr * xr - wdi * xi
		          yi = wdr * xi + wdi * xr
		          a.Double(o4) = a.Double(o4) - yr
		          a.Double(o5) = a.Double(o5) - yi
		          a.Double(o8) = a.Double(o8) + yr
		          a.Double(o7) =  a.Double(o7) - yi
		          wkr = wkr + (ss * wdi)
		          wki = wki  + (ss * (0.5 - wdr))
		          xr = a.Double(o1) - a.Double(o2)
		          xi = a.Double(o3) + a.Double(o6)
		          yr = wkr * xr - wki * xi
		          yi = wkr * xi + wki * xr
		          a.Double(o1) =  a.Double(o1) - yr
		          a.Double(o3) = a.Double(o3) - yi
		          a.Double(o2) = a.Double(o2) + yr
		          a.Double(o6) = a.Double(o6) - yi
		          wdr = wdr + (ss * wki)
		          wdi  = wdi + (ss * (0.5 - wkr))
		        Next
		        If (i0 = 4)  Then Exit
		        wkr = 0.5 * sin(ec * i0)
		        wki = 0.5 * cos(ec * i0)
		        wdr = 0.5 - (wkr * w1r - wki * w1i)
		        wdi = wkr * w1i + wki * w1r
		        wkr = 0.5 - wkr
		        i = i0
		      Wend
		      o1 = (power_of_two - 1) * 8
		      o2 = o1 -  8
		      xr = a.Double(16) - a.Double(o2)
		      xi = a.Double(24) + a.Double(o1)
		      yr = wdr * xr - wdi * xi
		      yi = wdr * xi + wdi * xr
		      a.Double(16) =  a.Double(16) - yr
		      a.Double(24) =  a.Double(24) - yi
		      a.Double(o2) =  a.Double(o2) + yr
		      a.Double(o1) =  a.Double(o1) - yi
		    End If
		    xi = a.Double(0) - a.Double(8)
		    a.Double(0) = a.Double(0) + a.Double(8)
		    a.Double(8) = xi
		  Else
		    a.Double(8) = 0.5 * (a.Double(0) - a.Double(8))
		    a.Double(0) = a.Double(0) - a.Double(8)
		    If (power_of_two > 4) Then
		      ec = 2. * 1.570796326794896619231321691639751442098584699687 / power_of_two
		      wdi = cos(ec)
		      wdr = sin(ec)
		      wdi = wdi * wdr
		      wdr = wdr * wdr
		      w1r = 1. - 2. * wdr
		      w1i = 2. * wdi
		      ss = 2. * w1i
		      i = bitwise.ShiftRight(power_of_two, 1)
		      o1 = (i + 1) * 8
		      a.Double(o1) = -a.Double(o1)
		      While True
		        i0 = i - 256
		        If (i0 < 4) Then i0 = 4
		        For j = i - 4 DownTo i0 Step 4
		          k = power_of_two - j
		          o1 = j * 8
		          o2 = k * 8
		          o3 = o1 + 8
		          o4 = o3 + 8
		          o5 = o4 + 8
		          o6 = o2 + 8
		          o7 = o2 - 8
		          o8 = o7 - 8
		          xr = a.Double(o4) - a.Double(o8)
		          xi = a.Double(o5) + a.Double(o7)
		          yr = wdr * xr + wdi * xi
		          yi = wdr * xi - wdi * xr
		          a.Double(o4) = a.Double(o4) - yr
		          a.Double(o5) = yi - a.Double(o5)
		          a.Double(o8) =  a.Double(o8) + yr
		          a.Double(o7) = yi - a.Double(o7)
		          wkr = wkr + (ss * wdi)
		          wki = wki + (ss * (0.5 - wdr))
		          xr = a.Double(o1) - a.Double(o2)
		          xi = a.Double(o3) + a.Double(o6)
		          yr = wkr * xr + wki * xi
		          yi = wkr * xi - wki * xr
		          a.Double(o1) = a.Double(o1)  - yr
		          a.Double(o3) = yi - a.Double(o3)
		          a.Double(o2) =  a.Double(o2) + yr
		          a.Double(o6) = yi - a.Double(o6)
		          wdr = wdr + (ss * wki)
		          wdi = wdi + (ss * (0.5 - wkr))
		        Next
		        If (i0 = 4) Then Exit
		        wkr = 0.5 * sin(ec * i0)
		        wki = 0.5 * cos(ec * i0)
		        wdr = 0.5 - (wkr * w1r - wki * w1i)
		        wdi = wkr * w1i + wki * w1r
		        wkr = 0.5 - wkr
		        i = i0
		      Wend
		      o1 = (power_of_two - 1) * 8
		      o2 = o1 -  8
		      xr = a.Double(16) - a.Double(o2)
		      xi = a.Double(24) + a.Double(o1)
		      yr = wdr * xr + wdi * xi
		      yi = wdr * xi - wdi * xr
		      a.Double(16) = a.Double(16) - yr
		      a.Double(24) = yi - a.Double(24)
		      a.Double(o2) =  a.Double(o2) + yr
		      a.Double(o1) = yi - a.Double(o1)
		      a.Double(8) = -a.Double(8)
		      l = bitwise.ShiftRight(power_of_two, 2)
		      m = 2
		      While (m < l)
		        l = bitwise.ShiftRight(l, 1)
		        m = bitwise.ShiftLeft(m, 1)
		      Wend
		      If (m = l) Then
		        j0 = 0
		        l1 = m - 1
		        For k0 = 0 To l1 Step 2
		          k = k0
		          l2 = j0 + k0 - 1
		          For j = j0 To l2 Step 2
		            o1 = j * 8
		            o2 = o1 + 8
		            o3 = k * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j + m
		            k1 = k + 2 * m
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j1 + m
		            k1 = k1 - m
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j1 + m
		            k1 = k1 + (2 * m)
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 +  8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            i = bitwise.ShiftRight(power_of_two, 1)
		            k = k Xor i
		            While i > k
		              i = bitwise.ShiftRight(i, 1)
		              k = k Xor i
		            Wend
		          Next
		          j1 = j0 + k0 + m
		          k1 = j1 + m
		          o1 = j1 * 8
		          o2 = o1 + 8
		          o3 = k1 * 8
		          o4 = o3 + 8
		          xr = a.double(o1)
		          xi = a.double(o2)
		          yr = a.double(o3)
		          yi = a.double(o4)
		          a.double(o1) = yr
		          a.double(o2) = yi
		          a.double(o3) = xr
		          a.double(o4) = xi
		          
		          i = bitwise.ShiftRight(power_of_two, 1)
		          j0 = j0 Xor i
		          While i > j0
		            i = bitwise.ShiftRight(i, 1)
		            j0 = j0 Xor i
		          Wend
		        Next
		      Else
		        j0 = 0
		        l1 = m - 1
		        For k0 = 2 To l1 Step 2
		          i = bitwise.ShiftRight(power_of_two, 1)
		          j0 = j0 Xor i
		          While i > j0
		            i = bitwise.ShiftRight(i, 1)
		            j0 = j0 Xor i
		          Wend
		          k = k0
		          l2 = j0 + k0 -1
		          For j = j0 To l2 Step 2
		            o1 = j * 8
		            o2 = o1 + 8
		            o3 = k * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            j1 = j + m
		            k1 = k + m
		            o1 = j1 * 8
		            o2 = o1 + 8
		            o3 = k1 * 8
		            o4 = o3 + 8
		            xr = a.double(o1)
		            xi = a.double(o2)
		            yr = a.double(o3)
		            yi = a.double(o4)
		            a.double(o1) = yr
		            a.double(o2) = yi
		            a.double(o3) = xr
		            a.double(o4) = xi
		            i = bitwise.ShiftRight(power_of_two, 1)
		            k = k Xor i
		            While i > k
		              i = bitwise.ShiftRight(i, 1)
		              k = k Xor i
		            Wend
		          Next
		        Next
		      End If
		      l = 2
		      If (power_of_two > 8) Then
		        x0r = a.Double(0) + a.Double(16)
		        x0i = a.Double(8) + a.Double(24)
		        x1r = a.Double(0) - a.Double(16)
		        x1i = a.Double(8) - a.Double(24)
		        x2r = a.Double(32) + a.Double(48)
		        x2i = a.Double(40) + a.Double(56)
		        x3r = a.Double(32) - a.Double(48)
		        x3i = a.Double(40) - a.Double(56)
		        a.Double(0) = x0r + x2r
		        a.Double(8) = x0i + x2i
		        a.Double(32) = x0r - x2r
		        a.Double(40) = x0i - x2i
		        a.Double(16) = x1r - x3i
		        a.Double(24) = x1i + x3r
		        a.Double(48) = x1r + x3i
		        a.Double(56) = x1i - x3r
		        wn4r = 0.707106781186547524400844362104849039284835937688  //cos(pi_2*0.5)
		        x0r = a.Double(64) + a.Double(80)
		        x0i = a.Double(72) + a.Double(88)
		        x1r = a.Double(64) - a.Double(80)
		        x1i = a.Double(72) - a.Double(88)
		        x2r = a.Double(96) + a.Double(112)
		        x2i = a.Double(104) + a.Double(120)
		        x3r = a.Double(96) - a.Double(112)
		        x3i = a.Double(104) - a.Double(120)
		        a.Double(64) = x0r + x2r
		        a.Double(72) = x0i + x2i
		        a.Double(96) = x2i - x0i
		        a.Double(104) = x0r - x2r
		        x0r = x1r - x3i
		        x0i = x1i + x3r
		        a.Double(80) = wn4r * (x0r - x0i)
		        a.Double(88) = wn4r * (x0r + x0i)
		        x0r = x3i + x1r
		        x0i = x3r - x1i
		        a.Double(112) = wn4r * (x0i - x0r)
		        a.Double(120) = wn4r * (x0i + x0r)
		        ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		        kr = 0
		        l1 = power_of_two - 1
		        For j = 16 To l1 Step 16
		          kj = bitwise.ShiftRight(power_of_two, 2)
		          kr = kr Xor kj
		          While (kj > kr)
		            kj = bitwise.ShiftRight(kj, 1)
		            kr = kr Xor kj
		          Wend
		          wk1r = cos(ew * kr)
		          wk1i = sin(ew * kr)
		          wk2r = 1 - 2 * wk1i * wk1i
		          wk2i = 2 * wk1i * wk1r
		          wk3r = wk1r - 2 * wk2i * wk1i
		          wk3i = 2 * wk2i * wk1r - wk1i
		          o0 = j * 8
		          o1 = o0 + 8
		          o2 = o1 + 8
		          o3 = o2 + 8
		          o4 = o3 + 8
		          o5 = o4 + 8
		          o6 = o5 + 8
		          o7 = o6 + 8
		          x0r = a.Double(o0) + a.Double(o2)
		          x0i = a.Double(o1) + a.Double(o3)
		          x1r = a.Double(o0) - a.Double(o2)
		          x1i = a.Double(o1) - a.Double(o3)
		          x2r = a.Double(o4) + a.Double(o6)
		          x2i = a.Double(o5) + a.Double(o7)
		          x3r = a.Double(o4) - a.Double(o6)
		          x3i = a.Double(o5) - a.Double(o7)
		          a.Double(o0) = x0r + x2r
		          a.Double(o1) = x0i + x2i
		          x0r = x0r  - x2r
		          x0i = x0i - x2i
		          a.Double(o4) = wk2r * x0r - wk2i * x0i
		          a.Double(o5) = wk2r * x0i + wk2i * x0r
		          x0r = x1r - x3i
		          x0i = x1i + x3r
		          a.Double(o2) = wk1r * x0r - wk1i * x0i
		          a.Double(o3) = wk1r * x0i + wk1i * x0r
		          x0r = x1r + x3i
		          x0i = x1i - x3r
		          a.Double(o6) = wk3r * x0r - wk3i * x0i
		          a.Double(o7) = wk3r * x0i + wk3i * x0r
		          x0r = wn4r * (wk1r - wk1i)
		          wk1i = wn4r * (wk1r + wk1i)
		          wk1r = x0r
		          wk3r = wk1r - 2 * wk2r * wk1i
		          wk3i = 2 * wk2r * wk1r - wk1i
		          o8 = (j + 8) * 8
		          o9 = o8 + 8
		          o10 = o9 + 8
		          o11 = o10 + 8
		          o12 = o11 + 8
		          o13 = o12 + 8
		          o14 = o13 + 8
		          o15 = o14 + 8
		          x0r = a.Double(o8) + a.Double(o10)
		          x0i = a.Double(o9) + a.Double(o11)
		          x1r = a.Double(o8) - a.Double(o10)
		          x1i = a.Double(o9) - a.Double(o11)
		          x2r = a.Double(o12) + a.Double(o14)
		          x2i = a.Double(o13) + a.Double(o15)
		          x3r = a.Double(o12) - a.Double(o14)
		          x3i = a.Double(o13) - a.Double(o15)
		          a.Double(o8) = x0r + x2r
		          a.Double(o9) = x0i + x2i
		          x0r = x0r - x2r
		          x0i = x0i - x2i
		          a.Double(o12) = -wk2i * x0r - wk2r * x0i
		          a.Double(o13) = -wk2i * x0i + wk2r * x0r
		          x0r = x1r - x3i
		          x0i = x1i + x3r
		          a.Double(o10) = wk1r * x0r - wk1i * x0i
		          a.Double(o11) = wk1r * x0i + wk1i * x0r
		          x0r = x1r + x3i
		          x0i = x1i - x3r
		          a.Double(o14) = wk3r * x0r - wk3i * x0i
		          a.Double(o15) = wk3r * x0i + wk3i * x0r
		        Next
		        l = 8
		        While (bitwise.ShiftLeft(l, 2) < power_of_two)
		          m = bitwise.ShiftLeft(l, 2)
		          l1 = l - 1
		          For j = 0 To  l1 Step 2
		            j1 = j + l
		            j2 = j1 + l
		            j3 = j2 + l
		            o1 = j * 8
		            o2 = j1 * 8
		            o3 = j2 * 8
		            o4 = j3 * 8
		            o5 = o1 + 8
		            o6 = o2 + 8
		            o7 = o3 + 8
		            o8 = o4 + 8
		            x0r = a.Double(o1) + a.Double(o2)
		            x0i = a.Double(o5) + a.Double(o6)
		            x1r = a.Double(o1) - a.Double(o2)
		            x1i = a.Double(o5) - a.Double(o6)
		            x2r = a.Double(o3) + a.Double(o4)
		            x2i = a.Double(o7) + a.Double(o8)
		            x3r = a.Double(o3) - a.Double(o4)
		            x3i = a.Double(o7) - a.Double(o8)
		            a.Double(o1) = x0r + x2r
		            a.Double(o5) = x0i + x2i
		            a.Double(o3) = x0r - x2r
		            a.Double(o7) = x0i - x2i
		            a.Double(o2) = x1r - x3i
		            a.Double(o6) = x1i + x3r
		            a.Double(o4) = x1r + x3i
		            a.Double(o8) = x1i - x3r
		          Next
		          wn4r = 0.707106781186547524400844362104849039284835937688
		          l1 = l + m - 1
		          For j = m To l1 Step 2
		            j1 = j + l
		            j2 = j1 + l
		            j3 = j2 + l
		            o1 = j * 8
		            o2 = j1 * 8
		            o3 = j2 * 8
		            o4 = j3 * 8
		            o5 = o1 + 8
		            o6 = o2 + 8
		            o7 = o3 + 8
		            o8 = o4 + 8
		            x0r = a.Double(o1) + a.Double(o2)
		            x0i = a.Double(o5) + a.Double(o6)
		            x1r = a.Double(o1) - a.Double(o2)
		            x1i = a.Double(o5) - a.Double(o6)
		            x2r = a.Double(o3) + a.Double(o4)
		            x2i = a.Double(o7) + a.Double(o8)
		            x3r = a.Double(o3) - a.Double(o4)
		            x3i = a.Double(o7) - a.Double(o8)
		            a.Double(o1) = x0r + x2r
		            a.Double(o5) = x0i + x2i
		            a.Double(o3) = x2i - x0i
		            a.Double(o7) = x0r - x2r
		            x0r = x1r - x3i
		            x0i = x1i + x3r
		            a.Double(o2) = wn4r * (x0r - x0i)
		            a.Double(o6) = wn4r * (x0r + x0i)
		            x0r = x3i + x1r
		            x0i = x3r - x1i
		            a.Double(o4) = wn4r * (x0i - x0r)
		            a.Double(o8) = wn4r * (x0i + x0r)
		          Next
		          ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		          kr = 0
		          m2 = 2 * m
		          l1 = power_of_two - 1
		          For k = m2 To l1 Step m2
		            kj = bitwise.ShiftRight(power_of_two, 2)
		            kr = kr Xor kj
		            While kj > kr
		              kj = bitwise.ShiftRight(kj, 1)
		              kr = kr Xor kj
		            Wend
		            wk1r = cos(ew * kr)
		            wk1i = sin(ew * kr)
		            wk2r = 1 - 2 * wk1i * wk1i
		            wk2i = 2 * wk1i * wk1r
		            wk3r = wk1r - 2 * wk2i * wk1i
		            wk3i = 2 * wk2i * wk1r - wk1i
		            l2 = l + k - 1
		            For j = k To l2 Step 2
		              j1 = j + l
		              j2 = j1 + l
		              j3 = j2 + l
		              o1 = j * 8
		              o2 = j1 * 8
		              o3 = j2 * 8
		              o4 = j3 * 8
		              o5 = o1 + 8
		              o6 = o2 + 8
		              o7 = o3 + 8
		              o8 = o4 + 8
		              x0r = a.Double(o1) + a.Double(o2)
		              x0i = a.Double(o5) + a.Double(o6)
		              x1r = a.Double(o1) - a.Double(o2)
		              x1i = a.Double(o5) - a.Double(o6)
		              x2r = a.Double(o3) + a.Double(o4)
		              x2i = a.Double(o7) + a.Double(o8)
		              x3r = a.Double(o3) - a.Double(o4)
		              x3i = a.Double(o7) - a.Double(o8)
		              a.Double(o1) = x0r + x2r
		              a.Double(o5) = x0i + x2i
		              x0r = x0r - x2r
		              x0i = x0i - x2i
		              a.Double(o3) = wk2r * x0r - wk2i * x0i
		              a.Double(o7) = wk2r * x0i + wk2i * x0r
		              x0r = x1r - x3i
		              x0i = x1i + x3r
		              a.Double(o2) = wk1r * x0r - wk1i * x0i
		              a.Double(o6) = wk1r * x0i + wk1i * x0r
		              x0r = x1r + x3i
		              x0i = x1i - x3r
		              a.Double(o4) = wk3r * x0r - wk3i * x0i
		              a.Double(o8) = wk3r * x0i + wk3i * x0r
		            Next
		            x0r = wn4r * (wk1r - wk1i)
		            wk1i = wn4r * (wk1r + wk1i)
		            wk1r = x0r
		            wk3r = wk1r - 2 * wk2r * wk1i
		            wk3i = 2 * wk2r * wk1r - wk1i
		            l2 = l + (k + m) - 1
		            For j = k + m To l2 Step 2
		              j1 = j + l
		              j2 = j1 + l
		              j3 = j2 + l
		              o1 = j * 8
		              o2 = j1 * 8
		              o3 = j2 * 8
		              o4 = j3 * 8
		              o5 = o1 + 8
		              o6 = o2 + 8
		              o7 = o3 + 8
		              o8 = o4 + 8
		              x0r = a.Double(o1) + a.Double(o2)
		              x0i = a.Double(o5) + a.Double(o6)
		              x1r = a.Double(o1) - a.Double(o2)
		              x1i = a.Double(o5) - a.Double(o6)
		              x2r = a.Double(o3) + a.Double(o4)
		              x2i = a.Double(o7) + a.Double(o8)
		              x3r = a.Double(o3) - a.Double(o4)
		              x3i = a.Double(o7) - a.Double(o8)
		              a.Double(o1) = x0r + x2r
		              a.Double(o5) = x0i + x2i
		              x0r = x0r - x2r
		              x0i = x0i - x2i
		              a.Double(o3) = -wk2i * x0r - wk2r * x0i
		              a.Double(o7) = -wk2i * x0i + wk2r * x0r
		              x0r = x1r - x3i
		              x0i = x1i + x3r
		              a.Double(o2) = wk1r * x0r - wk1i * x0i
		              a.Double(o6) = wk1r * x0i + wk1i * x0r
		              x0r = x1r + x3i
		              x0i = x1i - x3r
		              a.Double(o4) = wk3r * x0r - wk3i * x0i
		              a.Double(o8) = wk3r * x0i + wk3i * x0r
		            Next
		          Next
		          l = bitwise.ShiftLeft(l, 2)
		        Wend
		      End If
		      If bitwise.ShiftLeft(l, 2) = power_of_two Then
		        l1 = l - 1
		        For j = 0 To l1 Step 2
		          j1 = j + l
		          j2 = j1 + l
		          j3 = j2 + l
		          o1 = j * 8
		          o2 = j1 * 8
		          o3 = j2 * 8
		          o4 = j3 * 8
		          o5 = o1 + 8
		          o6 = o2 + 8
		          o7 = o3 + 8
		          o8 = o4 + 8
		          x0r = a.Double(o1) + a.Double(o2)
		          x0i = -a.Double(o5) - a.Double(o6)
		          x1r = a.Double(o1) - a.Double(o2)
		          x1i = -a.Double(o5) + a.Double(o6)
		          x2r = a.Double(o3) + a.Double(o4)
		          x2i = a.Double(o7) + a.Double(o8)
		          x3r = a.Double(o3) - a.Double(o4)
		          x3i = a.Double(o7) - a.Double(o8)
		          a.Double(o1) = x0r + x2r
		          a.Double(o5) = x0i - x2i
		          a.Double(o3) = x0r - x2r
		          a.Double(o7) = x0i + x2i
		          a.Double(o2)= x1r - x3i
		          a.Double(o6) = x1i - x3r
		          a.Double(o4) = x1r + x3i
		          a.Double(o8) = x1i + x3r
		        Next
		      Else
		        l1 = l - 1
		        For j = 0 To l1 Step 2
		          j1 = j + l
		          o1 = j * 8
		          o2 = j1 * 8
		          o3 = o1 + 8
		          o4 = o2 +  8
		          x0r = a.Double(o1) - a.Double(o2)
		          x0i = -a.Double(o3) + a.Double(o4)
		          a.Double(o1) = a.Double(o1) + a.Double(o2)
		          a.Double(o3) = -a.Double(o3) - a.Double(o4)
		          a.Double(o2) = x0r
		          a.Double(o4) = x0i
		        Next
		      End If
		    Elseif (power_of_two = 4) Then
		      l = 2
		      If (power_of_two > 8) Then
		        x0r = a.Double(0) + a.Double(16)
		        x0i = a.Double(8) + a.Double(24)
		        x1r = a.Double(0) - a.Double(16)
		        x1i = a.Double(8) - a.Double(24)
		        x2r = a.Double(32) + a.Double(48)
		        x2i = a.Double(40) + a.Double(56)
		        x3r = a.Double(32) - a.Double(48)
		        x3i = a.Double(40) - a.Double(56)
		        a.Double(0) = x0r + x2r
		        a.Double(8) = x0i + x2i
		        a.Double(32) = x0r - x2r
		        a.Double(40) = x0i - x2i
		        a.Double(16) = x1r - x3i
		        a.Double(24) = x1i + x3r
		        a.Double(48) = x1r + x3i
		        a.Double(56) = x1i - x3r
		        wn4r = 0.707106781186547524400844362104849039284835937688  //cos(pi_2*0.5)
		        x0r = a.Double(64) + a.Double(80)
		        x0i = a.Double(72) + a.Double(88)
		        x1r = a.Double(64) - a.Double(80)
		        x1i = a.Double(72) - a.Double(88)
		        x2r = a.Double(96) + a.Double(112)
		        x2i = a.Double(104) + a.Double(120)
		        x3r = a.Double(96) - a.Double(112)
		        x3i = a.Double(104) - a.Double(120)
		        a.Double(64) = x0r + x2r
		        a.Double(72) = x0i + x2i
		        a.Double(96) = x2i - x0i
		        a.Double(104) = x0r - x2r
		        x0r = x1r - x3i
		        x0i = x1i + x3r
		        a.Double(80) = wn4r * (x0r - x0i)
		        a.Double(88) = wn4r * (x0r + x0i)
		        x0r = x3i + x1r
		        x0i = x3r - x1i
		        a.Double(112) = wn4r * (x0i - x0r)
		        a.Double(120) = wn4r * (x0i + x0r)
		        ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		        kr = 0
		        l1 = power_of_two - 1
		        For j = 16 To l1 Step 16
		          kj = bitwise.ShiftRight(power_of_two, 2)
		          kr = kr Xor kj
		          While (kj > kr)
		            kj = bitwise.ShiftRight(kj, 1)
		            kr = kr Xor kj
		          Wend
		          wk1r = cos(ew * kr)
		          wk1i = sin(ew * kr)
		          wk2r = 1 - 2 * wk1i * wk1i
		          wk2i = 2 * wk1i * wk1r
		          wk3r = wk1r - 2 * wk2i * wk1i
		          wk3i = 2 * wk2i * wk1r - wk1i
		          o0 = j * 8
		          o1 = o0 + 8
		          o2 = o1 + 8
		          o3 = o2 + 8
		          o4 = o3 + 8
		          o5 = o4 + 8
		          o6 = o5 + 8
		          o7 = o6 + 8
		          x0r = a.Double(o0) + a.Double(o2)
		          x0i = a.Double(o1) + a.Double(o3)
		          x1r = a.Double(o0) - a.Double(o2)
		          x1i = a.Double(o1) - a.Double(o3)
		          x2r = a.Double(o4) + a.Double(o6)
		          x2i = a.Double(o5) + a.Double(o7)
		          x3r = a.Double(o4) - a.Double(o6)
		          x3i = a.Double(o5) - a.Double(o7)
		          a.Double(o0) = x0r + x2r
		          a.Double(o1) = x0i + x2i
		          x0r = x0r  - x2r
		          x0i = x0i - x2i
		          a.Double(o4) = wk2r * x0r - wk2i * x0i
		          a.Double(o5) = wk2r * x0i + wk2i * x0r
		          x0r = x1r - x3i
		          x0i = x1i + x3r
		          a.Double(o2) = wk1r * x0r - wk1i * x0i
		          a.Double(o3) = wk1r * x0i + wk1i * x0r
		          x0r = x1r + x3i
		          x0i = x1i - x3r
		          a.Double(o6) = wk3r * x0r - wk3i * x0i
		          a.Double(o7) = wk3r * x0i + wk3i * x0r
		          x0r = wn4r * (wk1r - wk1i)
		          wk1i = wn4r * (wk1r + wk1i)
		          wk1r = x0r
		          wk3r = wk1r - 2 * wk2r * wk1i
		          wk3i = 2 * wk2r * wk1r - wk1i
		          o8 = (j + 8) * 8
		          o9 = o8 + 8
		          o10 = o9 + 8
		          o11 = o10 + 8
		          o12 = o11 + 8
		          o13 = o12 + 8
		          o14 = o13 + 8
		          o15 = o14 + 8
		          x0r = a.Double(o8) + a.Double(o10)
		          x0i = a.Double(o9) + a.Double(o11)
		          x1r = a.Double(o8) - a.Double(o10)
		          x1i = a.Double(o9) - a.Double(o11)
		          x2r = a.Double(o12) + a.Double(o14)
		          x2i = a.Double(o13) + a.Double(o15)
		          x3r = a.Double(o12) - a.Double(o14)
		          x3i = a.Double(o13) - a.Double(o15)
		          a.Double(o8) = x0r + x2r
		          a.Double(o9) = x0i + x2i
		          x0r = x0r - x2r
		          x0i = x0i - x2i
		          a.Double(o12) = -wk2i * x0r - wk2r * x0i
		          a.Double(o13) = -wk2i * x0i + wk2r * x0r
		          x0r = x1r - x3i
		          x0i = x1i + x3r
		          a.Double(o10) = wk1r * x0r - wk1i * x0i
		          a.Double(o11) = wk1r * x0i + wk1i * x0r
		          x0r = x1r + x3i
		          x0i = x1i - x3r
		          a.Double(o14) = wk3r * x0r - wk3i * x0i
		          a.Double(o15) = wk3r * x0i + wk3i * x0r
		        Next
		        l = 8
		        While ( bitwise.ShiftLeft(l, 2) < power_of_two)
		          m = bitwise.ShiftLeft(l, 2)
		          l1 = l - 1
		          For j = 0 To  l1 Step 2
		            j1 = j + l
		            j2 = j1 + l
		            j3 = j2 + l
		            o1 = j * 8
		            o2 = j1 * 8
		            o3 = j2 * 8
		            o4 = j3 * 8
		            o5 = o1 + 8
		            o6 = o2 + 8
		            o7 = o3 + 8
		            o8 = o4 + 8
		            x0r = a.Double(o1) + a.Double(o2)
		            x0i = a.Double(o5) + a.Double(o6)
		            x1r = a.Double(o1) - a.Double(o2)
		            x1i = a.Double(o5) - a.Double(o6)
		            x2r = a.Double(o3) + a.Double(o4)
		            x2i = a.Double(o7) + a.Double(o8)
		            x3r = a.Double(o3) - a.Double(o4)
		            x3i = a.Double(o7) - a.Double(o8)
		            a.Double(o1) = x0r + x2r
		            a.Double(o5) = x0i + x2i
		            a.Double(o3) = x0r - x2r
		            a.Double(o7) = x0i - x2i
		            a.Double(o2) = x1r - x3i
		            a.Double(o6) = x1i + x3r
		            a.Double(o4) = x1r + x3i
		            a.Double(o8) = x1i - x3r
		          Next
		          wn4r = 0.707106781186547524400844362104849039284835937688
		          l1 = l + m - 1
		          For j = m To l1 Step 2
		            j1 = j + l
		            j2 = j1 + l
		            j3 = j2 + l
		            o1 = j * 8
		            o2 = j1 * 8
		            o3 = j2 * 8
		            o4 = j3 * 8
		            o5 = o1 + 8
		            o6 = o2 + 8
		            o7 = o3 + 8
		            o8 = o4 + 8
		            x0r = a.Double(o1) + a.Double(o2)
		            x0i = a.Double(o5) + a.Double(o6)
		            x1r = a.Double(o1) - a.Double(o2)
		            x1i = a.Double(o5) - a.Double(o6)
		            x2r = a.Double(o3) + a.Double(o4)
		            x2i = a.Double(o7) + a.Double(o8)
		            x3r = a.Double(o3) - a.Double(o4)
		            x3i = a.Double(o7) - a.Double(o8)
		            a.Double(o1) = x0r + x2r
		            a.Double(o5) = x0i + x2i
		            a.Double(o3) = x2i - x0i
		            a.Double(o7) = x0r - x2r
		            x0r = x1r - x3i
		            x0i = x1i + x3r
		            a.Double(o2) = wn4r * (x0r - x0i)
		            a.Double(o6) = wn4r * (x0r + x0i)
		            x0r = x3i + x1r
		            x0i = x3r - x1i
		            a.Double(o4) = wn4r * (x0i - x0r)
		            a.Double(o8) = wn4r * (x0i + x0r)
		          Next
		          ew = 1.570796326794896619231321691639751442098584699687 / power_of_two
		          kr = 0
		          m2 = 2 * m
		          l1 = power_of_two - 1
		          For k = m2 To l1 Step m2
		            
		            kj = bitwise.ShiftRight(power_of_two, 2)
		            kr = kr Xor kj
		            While kj > kr
		              kj = bitwise.ShiftRight(kj, 1)
		              kr = kr Xor kj
		            Wend
		            wk1r = cos(ew * kr)
		            wk1i = sin(ew * kr)
		            wk2r = 1 - 2 * wk1i * wk1i
		            wk2i = 2 * wk1i * wk1r
		            wk3r = wk1r - 2 * wk2i * wk1i
		            wk3i = 2 * wk2i * wk1r - wk1i
		            l2 = l + k - 1
		            For j = k To l2 Step 2
		              j1 = j + l
		              j2 = j1 + l
		              j3 = j2 + l
		              o1 = j * 8
		              o2 = j1 * 8
		              o3 = j2 * 8
		              o4 = j3 * 8
		              o5 = o1 + 8
		              o6 = o2 + 8
		              o7 = o3 + 8
		              o8 = o4 + 8
		              x0r = a.Double(o1) + a.Double(o2)
		              x0i = a.Double(o5) + a.Double(o6)
		              x1r = a.Double(o1) - a.Double(o2)
		              x1i = a.Double(o5) - a.Double(o6)
		              x2r = a.Double(o3) + a.Double(o4)
		              x2i = a.Double(o7) + a.Double(o8)
		              x3r = a.Double(o3) - a.Double(o4)
		              x3i = a.Double(o7) - a.Double(o8)
		              a.Double(o1) = x0r + x2r
		              a.Double(o5) = x0i + x2i
		              x0r = x0r - x2r
		              x0i = x0i - x2i
		              a.Double(o3) = wk2r * x0r - wk2i * x0i
		              a.Double(o7) = wk2r * x0i + wk2i * x0r
		              x0r = x1r - x3i
		              x0i = x1i + x3r
		              a.Double(o2) = wk1r * x0r - wk1i * x0i
		              a.Double(o6) = wk1r * x0i + wk1i * x0r
		              x0r = x1r + x3i
		              x0i = x1i - x3r
		              a.Double(o4) = wk3r * x0r - wk3i * x0i
		              a.Double(o8) = wk3r * x0i + wk3i * x0r
		            Next
		            x0r = wn4r * (wk1r - wk1i)
		            wk1i = wn4r * (wk1r + wk1i)
		            wk1r = x0r
		            wk3r = wk1r - 2 * wk2r * wk1i
		            wk3i = 2 * wk2r * wk1r - wk1i
		            l2 = l + (k + m) - 1
		            For j = k + m To l2 Step 2
		              j1 = j + l
		              j2 = j1 + l
		              j3 = j2 + l
		              o1 = j * 8
		              o2 = j1 * 8
		              o3 = j2 * 8
		              o4 = j3 * 8
		              o5 = o1 + 8
		              o6 = o2 + 8
		              o7 = o3 + 8
		              o8 = o4 + 8
		              x0r = a.Double(o1) + a.Double(o2)
		              x0i = a.Double(o5) + a.Double(o6)
		              x1r = a.Double(o1) - a.Double(o2)
		              x1i = a.Double(o5) - a.Double(o6)
		              x2r = a.Double(o3) + a.Double(o4)
		              x2i = a.Double(o7) + a.Double(o8)
		              x3r = a.Double(o3) - a.Double(o4)
		              x3i = a.Double(o7) - a.Double(o8)
		              a.Double(o1) = x0r + x2r
		              a.Double(o5) = x0i + x2i
		              x0r = x0r - x2r
		              x0i = x0i - x2i
		              a.Double(o3) = -wk2i * x0r - wk2r * x0i
		              a.Double(o7) = -wk2i * x0i + wk2r * x0r
		              x0r = x1r - x3i
		              x0i = x1i + x3r
		              a.Double(o2) = wk1r * x0r - wk1i * x0i
		              a.Double(o6) = wk1r * x0i + wk1i * x0r
		              x0r = x1r + x3i
		              x0i = x1i - x3r
		              a.Double(o4) = wk3r * x0r - wk3i * x0i
		              a.Double(o8) = wk3r * x0i + wk3i * x0r
		            Next
		          Next
		          l = bitwise.ShiftLeft(l, 2)
		        Wend
		      End If
		      If (bitwise.ShiftLeft(l, 2) = power_of_two) Then
		        l1 = l - 1
		        For j = 0 To l1 Step 2
		          j1 = j + l
		          j2 = j1 + l
		          j3 = j2 + l
		          o1 = j * 8
		          o2 = j1 * 8
		          o3 = j2 * 8
		          o4 = j3 * 8
		          o5 = o1 + 8
		          o6 = o2 + 8
		          o7 = o3 + 8
		          o8 = o4 + 8
		          x0r = a.Double(o1) + a.Double(o2)
		          x0i = a.Double(o5) + a.Double(o6)
		          x1r = a.Double(o1) - a.Double(o2)
		          x1i = a.Double(o5) - a.Double(o6)
		          x2r = a.Double(o3) + a.Double(o4)
		          x2i = a.Double(o7) + a.Double(o8)
		          x3r = a.Double(o3) - a.Double(o4)
		          x3i = a.Double(o7) - a.Double(o8)
		          a.Double(o1) = x0r + x2r
		          a.Double(o5) = x0i + x2i
		          a.Double(o3) = x0r - x2r
		          a.Double(o7) = x0i - x2i
		          a.Double(o2) = x1r - x3i
		          a.Double(o6) = x1i + x3r
		          a.Double(o4) = x1r + x3i
		          a.Double(o8) = x1i - x3r
		        Next
		      Else
		        l1 = l - 1
		        For j = 0 To  l1 Step 2
		          j1 = j + l
		          o1 = j * 8
		          o2 = j1 * 8
		          o3 = o1 + 8
		          o4 = o2 + 8
		          x0r = a.Double(o1) - a.Double(o2)
		          x0i = a.Double(o3) - a.Double(o4)
		          a.Double(o1) = a.Double(o1) + a.Double(o2)
		          a.Double(o3) = a.Double(o3) + a.Double(o4)
		          a.Double(o2) = x0r
		          a.Double(o4) = x0i
		        Next
		      End If
		    End If
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_rftbsub(power_of_two as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var i, i0, j, k As Int64
		  var o1, o2, o3, o4, o5, o6, o7, o8 As Int64
		  var ec, w1r, w1i, wkr, wki, wdr, wdi, ss, xr, xi, yr, yi As Double
		  
		  ec = 2. * 1.570796326794896619231321691639751442098584699687 / power_of_two
		  wdi = cos(ec)
		  wdr = sin(ec)
		  wdi = wdi * wdr
		  wdr = wdr * wdr
		  w1r = 1. - 2. * wdr
		  w1i = 2. * wdi
		  ss = 2. * w1i
		  i = power_of_two \ 2
		  o1 = (i + 1) * 8
		  a.Double(o1) = -a.Double(o1)
		  While True
		    i0 = i - 256
		    If (i0 < 4) Then i0 = 4
		    For j = i - 4 DownTo i0 Step 4
		      k = power_of_two - j
		      o1 = j * 8
		      o2 = k * 8
		      o3 = o1 + 8
		      o4 = o3 + 8
		      o5 = o4 + 8
		      o6 = o2 + 8
		      o7 = o2 - 8
		      o8 = o7 - 8
		      xr = a.Double(o4) - a.Double(o8)
		      xi = a.Double(o5) + a.Double(o7)
		      yr = wdr * xr + wdi * xi
		      yi = wdr * xi - wdi * xr
		      a.Double(o4) = a.Double(o4) - yr
		      a.Double(o5) = yi - a.Double(o5)
		      a.Double(o8) =  a.Double(o8) + yr
		      a.Double(o7) = yi - a.Double(o7)
		      wkr = wkr + (ss * wdi)
		      wki = wki + (ss * (0.5 - wdr))
		      xr = a.Double(o1) - a.Double(o2)
		      xi = a.Double(o3) + a.Double(o6)
		      yr = wkr * xr + wki * xi
		      yi = wkr * xi - wki * xr
		      a.Double(o1) = a.Double(o1)  - yr
		      a.Double(o3) = yi - a.Double(o3)
		      a.Double(o2) =  a.Double(o2) + yr
		      a.Double(o6) = yi - a.Double(o6)
		      wdr = wdr + (ss * wki)
		      wdi = wdi + (ss * (0.5 - wkr))
		    Next
		    If (i0 = 4) Then Exit
		    wkr = 0.5 * sin(ec * i0)
		    wki = 0.5 * cos(ec * i0)
		    wdr = 0.5 - (wkr * w1r - wki * w1i)
		    wdi = wkr * w1i + wki * w1r
		    wkr = 0.5 - wkr
		    i = i0
		  Wend
		  o1 = (power_of_two - 1) * 8
		  o2 = o1 -  8
		  xr = a.Double(16) - a.Double(o2)
		  xi = a.Double(24) + a.Double(o1)
		  yr = wdr * xr + wdi * xi
		  yi = wdr * xi - wdi * xr
		  a.Double(16) = a.Double(16) - yr
		  a.Double(24) = yi - a.Double(24)
		  a.Double(o2) =  a.Double(o2) + yr
		  a.Double(o1) = yi - a.Double(o1)
		  a.Double(8) = -a.Double(8)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21, CompatibilityFlags = API2Only
		Private Sub apmint_fft_rftfsub(power_of_two as int64, a as Ptr)
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  
		  var i, i0, j, k As Int64
		  var o1, o2, o3, o4, o5, o6, o7, o8 As Int64
		  var ec, w1r, w1i, wkr, wki, wdr, wdi, ss, xr, xi, yr, yi As Double
		  
		  ec = 2. * 1.570796326794896619231321691639751442098584699687 / power_of_two
		  wdi = cos(ec)
		  wdr = sin(ec)
		  wdi = wdi * wdr
		  wdr = wdr * wdr
		  w1r = 1. - 2. * wdr
		  w1i = 2. * wdi
		  ss = 2. * w1i
		  i = power_of_two \ 2
		  While True 
		    i0 = i - 256 //4 * 64
		    If (i0 < 4) Then i0 = 4
		    For j = i - 4 DownTo i0 Step 4
		      k = power_of_two - j
		      o1 = j * 8
		      o2 = k * 8
		      o3 = o1 + 8
		      o4 = o3 + 8
		      o5 = o4 + 8
		      o6 = o2 + 8
		      o7 = o2 - 8
		      o8 = o7 - 8
		      xr = a.Double(o4) - a.Double(o8)
		      xi = a.Double(o5) + a.Double(o7)
		      yr = wdr * xr - wdi * xi
		      yi = wdr * xi + wdi * xr
		      a.Double(o4) = a.Double(o4) - yr
		      a.Double(o5) = a.Double(o5) - yi
		      a.Double(o8) = a.Double(o8) + yr
		      a.Double(o7) =  a.Double(o7) - yi
		      wkr = wkr + (ss * wdi)
		      wki = wki  + (ss * (0.5 - wdr))
		      xr = a.Double(o1) - a.Double(o2)
		      xi = a.Double(o3) + a.Double(o6)
		      yr = wkr * xr - wki * xi
		      yi = wkr * xi + wki * xr
		      a.Double(o1) =  a.Double(o1) - yr
		      a.Double(o3) = a.Double(o3) - yi
		      a.Double(o2) = a.Double(o2) + yr
		      a.Double(o6) = a.Double(o6) - yi
		      wdr = wdr + (ss * wki)
		      wdi  = wdi + (ss * (0.5 - wkr))
		    Next
		    If (i0 = 4)  Then Exit
		    wkr = 0.5 * sin(ec * i0)
		    wki = 0.5 * cos(ec * i0)
		    wdr = 0.5 - (wkr * w1r - wki * w1i)
		    wdi = wkr * w1i + wki * w1r
		    wkr = 0.5 - wkr
		    i = i0
		  Wend
		  o1 = (power_of_two - 1) * 8
		  o2 = o1 -  8
		  xr = a.Double(16) - a.Double(o2)
		  xi = a.Double(24) + a.Double(o1)
		  yr = wdr * xr - wdi * xi
		  yi = wdr * xi + wdi * xr
		  a.Double(16) =  a.Double(16) - yr
		  a.Double(24) =  a.Double(24) - yi
		  a.Double(o2) =  a.Double(o2) + yr
		  a.Double(o1) =  a.Double(o1) - yi
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_from_int64(num as int64) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // converts an int64 to apmint
		  var result As New apmint_module.apmint
		  apmint_from_int64(num, result)
		  Return result
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_from_int64(num as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // converts a int64 to apmint
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  var ii As UInt64
		  
		  If num = 0 Then
		    output.Sign = 0
		    output.used = 0
		    Return
		  Elseif num > 0 Then
		    output.Sign = 1
		    ii = CType(num, UInt64)
		  Else
		    output.Sign = -1
		    If num = CType(-9223372036854775808, Int64) Then
		      ii = CType(9223372036854775808, UInt64)
		    Else
		      ii = CType(-num, UInt64)
		    End If
		  End If
		  
		  If output.digits Is Nil Or output.digits.size < 8 Then output.digits = New memoryblock(8)
		  output.digits.int64value(0) = ii
		  output.used = 8
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_from_string(s as string, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // contruct a new apmint from string
		  // strategy: buffer up digits in an int64 and when the in64 has 18 digits then move digits into result.
		  
		  if output is nil then output = new apmint_module.apmint
		  
		  var string_length As Int64 = s.Length
		  
		  If string_length = 0 Then   ' zero
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  var mb As memoryblock
		  mb =  s
		  var mbPtr As Ptr = mb
		  var b1 As Int8
		  var digits_found As Boolean
		  var int64_digits, int64_buffer, loopvar, digit, sign As Int64
		  var ten_to_18 As Int64 = 10 ^ 18
		  
		  // ignore characters in front that can't be start of number
		  var number_start As Int64 = -1
		  var byte_value As Int8
		  For loopvar = 0 To string_length - 1
		    byte_value = mbPtr.Int8(loopvar)
		    If (byte_value > 42 And byte_value < 58) And byte_value <> 44 And byte_value <> 46 And byte_value <> 47 Then
		      number_start = loopvar
		      Exit
		    End If
		  Next
		  
		  If number_start = -1 Then    ' zero
		    output.sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  output.sign = 0
		  output.used = 0
		  
		  sign = 1 ' default to positive value
		  
		  // process string
		  For loopvar = number_start To string_length-1
		    b1 = mbPtr.Int8(loopvar)
		    If b1 = 48 Then                                ' 0
		      If (digits_found) Then                       ' ignore leading zeros
		        int64_buffer = int64_buffer * 10
		        int64_digits = int64_digits + 1
		        If int64_digits > 17 Then
		          output = output * ten_to_18
		          apmint_add(output, int64_buffer, output)
		          int64_buffer = 0
		          int64_digits = 0
		        End If
		      End If
		    Elseif (b1 > 48) And (b1 < 58) Then           ' 1-9
		      digit = b1 - 48
		      int64_buffer = int64_buffer * 10 + digit
		      int64_digits = int64_digits + 1
		      If int64_digits > 17 Then
		        output = output * ten_to_18
		        apmint_add(output, int64_buffer, output)
		        int64_buffer = 0
		        int64_digits = 0
		      End If
		      digits_found = True
		    Elseif b1 = 43 Then                             ' +
		      Sign = 1
		    Elseif b1 = 45 Then                             ' -
		      Sign = -1
		    End If
		  Next
		  
		  // handle special case of all digits zero
		  If Not digits_found Then
		    output.sign = 0    ' zero
		    output.used = 0
		    Return
		  Else
		    // multiply by any remaining buffered digits
		    If int64_digits > 0 Then
		      output = output * CType(10 ^ int64_digits, Int64)
		      apmint_add(output, int64_buffer, output)
		    End If
		  End If
		  
		  output.sign = sign
		  
		  Return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_from_uint64(num as uint64) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // converts a uint64 to apmint
		  
		  var result As New apmint_module.apmint
		  apmint_from_uint64(num, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_from_uint64(num as uint64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // converts a uint64 to apmint, overwrites output
		  
		  If output is Nil Then output = New apmint_module.apmint
		  
		  If num = &h0 Then
		    output.sign = 0
		    output.used = 0
		    return
		  End If
		  
		  if output.digits is Nil or output.digits.size < 8 then output.digits = New memoryblock(8)
		  output.digits.uint64value(0) = num
		  output.used = 8
		  output.sign = 1
		  return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_gcd(aa as apmint_module.apmint, bb as apmint_module.apmint, byref gcd as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // greatest common divisor of two apmints
		  
		  var a as new apmint_module.apmint
		  var b as new apmint_module.apmint
		  
		  // a = abs(aa)
		  apmint_copy(aa, a)
		  a.sign = a.sign * a.sign
		  // b = abs(bb)
		  apmint_copy(bb, b)
		  b.sign = b.sign * b.sign
		  
		  While a.Sign <> 0
		    // b, b = b % a, a
		    apmint_modulo(b, a, b)
		    apmint_swap(a, b)
		  Wend
		  
		  apmint_copy(b, gcd)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_get_bit(input as apmint_module.apmint, bit_number as int64) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // returns a bit from apmint
		  // bit numbering begins with zero
		  
		  Static mask() As UInt8 = apmint_array_uint8(&h1, &h2, &h4, &h8, &h10, &h20, &h40, &h80)
		  
		  var byte_number As Int64 = bit_number \ 8
		  If bit_number < 0 Or Input Is Nil Or Input.Sign = 0 Or (byte_number + 1) > Input.used Then Return 0
		  
		  var ui8 As UInt8 = Input.digits.uint8value(byte_number) And mask(bit_number Mod 8)
		  If ui8 <> 0 Then
		    Return 1
		  Else
		    Return 0
		  End If
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_get_bits_clear(input as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // return the number zero bits in an apmint
		  
		  Return apmint_get_bit_length(input) - apmint_get_bits_set(input)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_get_bits_set(input as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  Static lookup_table() As Int8 = apmint_array_int8( _
		  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, _
		  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, _
		  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, _
		  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, _
		  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, _
		  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, _
		  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, _
		  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, _
		  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, _
		  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, _
		  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, _
		  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, _
		  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, _
		  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, _
		  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, _
		  4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8)
		  
		  // return the number of bits set in an apmint
		  
		  If Input Is Nil Or Input.Sign = 0 Then Return 0
		  
		  var offset As Int64 = Input.used
		  var iptr As ptr = Input.digits
		  var bits_set As Int64
		  
		  Do
		    offset = offset - 1
		    bits_set = bits_set + lookup_table(iptr.UInt8(offset))
		  Loop Until offset = 0
		  
		  Return bits_set
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_get_bit_length(input as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // return the number of bits in apmint
		  
		  Static lookup_table() As Int8 = apmint_array_int8( _
		  0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, _
		  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, _
		  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, _
		  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, _
		  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, _
		  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, _
		  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, _
		  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, _
		  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8)
		  
		  
		  If Input Is Nil Or Input.Sign = 0 Then Return 0  ' zero has no bits
		  
		  var offset As Int64 = Input.used
		  var iptr As ptr = Input.digits
		  var nzbyte As UInt8
		  
		  // find last non-zero byte
		  Do
		    offset = offset - 1
		    nzbyte = iptr.UInt8(offset)
		  Loop Until nzbyte <> 0
		  
		  
		  Return offset * 8 + lookup_table(nzbyte)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_get_first_bit_set(input as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  Static lookup_table() As Int8 = apmint_array_int8( _
		  -25, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, _
		  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0)
		  
		  // return the bit number of lowest bit that is set (-1 if no bit set)
		  
		  If Input Is Nil Or Input.Sign = 0 Then Return -1  ' zero has no set bits
		  
		  var offset As Int64 = -4
		  var iptr As ptr = Input.digits
		  var word32, byte_value As UInt32
		  var set_bit As Int64
		  
		  // number of bits of trailing 32 bit words that = 0
		  Do
		    offset = offset + 4
		    word32 = iptr.UInt32(offset)
		  Loop Until word32 <> 0
		  set_bit = offset * 8
		  
		  // add trailing bits of first non-zero 32 bit word
		  byte_value = word32 And &hff
		  If byte_value <> 0 Then Return set_bit + lookup_table(byte_value)
		  byte_value = (word32 \ &h100) And &hff
		  If byte_value <> 0 Then Return set_bit + lookup_table(byte_value) + 8
		  byte_value = (word32 \ &h10000) And &hff
		  If byte_value <> 0 Then Return set_bit + lookup_table(byte_value) + 16
		  byte_value = (word32 \ &h1000000) And &hff
		  Return set_bit + lookup_table(byte_value) + 24
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_get_least_significant_digit(input as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // returns the least significant base-10 digit of an apmint
		  
		  If input is nil or input.sign = 0 Then
		    Return 0
		  Else
		    Return apmint_to_int64(input Mod 10) * input.Sign
		  End If
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_grow(byref input as apmint_module.apmint, needed_length as int64, optional clear as Boolean = false)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // grow used space of apmint 'input' to 'needed_length'
		  // clear any new words added to used space if 'clear' flag is true
		  
		  if input is nil then input = new apmint_module.apmint
		  
		  If needed_length <= 0 Then  ' 0 length is a zero
		    input.Sign = 0
		    input.used = 0
		    Return
		  End If
		  
		  If input.digits Is Nil Then
		    input.digits = New memoryblock(needed_length)
		    input.used = needed_length
		    Return
		  End If
		  
		  If clear = True Then ' clear needed space if flag set
		    var clear_space as int64 = if (needed_length < input.digits.size, needed_length, input.digits.size)
		    If input.used < clear_space Then
		      var rp As ptr = input.digits
		      var offset As Int64 = clear_space - 8
		      While True
		        rp.UInt64(offset) = &h0
		        If offset <= input.used Then Exit
		      Wend
		    End If
		  End If
		  
		  If input.digits.size >= needed_length Then ' already long enough
		    input.used = needed_length
		    Return
		  End If
		  
		  // grow it
		  
		  input.digits.size = needed_length
		  input.used = needed_length
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_is_even(input as apmint_module.apmint) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // return true if ampint is even, false otherwise
		  
		  If Input Is Nil Or Input.Sign = 0 Then
		    Return True
		  Elseif ((Input.digits.UInt64Value(0) And &h1) = &h0) Then
		    Return True
		  Else
		    Return False
		  End If
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_is_likely_prime(input as apmint_module.apmint, optional passes as Int64 = 30) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // Returns true if apmint is likely to be a prime number, false if number is definitely not prime
		  // Strategy:
		  //
		  //  -  return false for input less than 2
		  //  -  return false for input divisible by 2
		  //  -  use passes of Miller-Raben primality test to provide very high confidence level of the number being prime or not.
		  //        For any non_prime (composite number), at least (3/4) of the numbers less than number will witness it being
		  //           composite.  So ... for k iterations, the probability of test failing is (1/4) ^ k or 4 ^ (-k)
		  //        For example, the default 30 iterations will produce a probability of test failure of 4 ^ -30.
		  //
		  //   For smaller input, a small number of Miller-Raben tests are required for a deterministic result.
		  //      if input < 1,373,653, it is enough to test against 2 and 3;
		  //      if input < 9,080,191, it is enough to test against 31 and 73;
		  //      if input  < 4,759,123,141, it is enough to test against 2, 7, and 61;
		  //
		  
		  Static one as apmint_module.apmint = 1
		  Static two as apmint_module.apmint = 2
		  Static three as apmint_module.apmint = 3
		  
		  If input = two or input = three Then Return True
		  If input < two Then Return False
		  If apmint_is_even(input) Then Return False
		  
		  // now perform Miller_Rabin primality test iterations to determine with high
		  // confidence whether number is prime or composite
		  
		  var a, d, n_minus1 As apmint_module.apmint
		  var s As New apmint_module.apmint
		  var is_prime As Boolean
		  
		  apmint_subtract(input, one, n_minus1)
		  apmint_copy(n_minus1, d)
		  
		  While apmint_is_even(d)
		    apmint_divide_by2(d, d)
		    apmint_add(s, one, s)
		  Wend
		  
		  //handle smaller numbers first where two or three tests are all that's required
		  
		  If input < "4759123141" Then
		    If input < "1373653" Then
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, two)
		      If is_prime = False Then Return False
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, three)
		      Return is_prime
		    Elseif input < "9080191" Then
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, "31")
		      If is_prime = False Then Return False
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, "73")
		      Return is_prime
		    Else
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, two)
		      If is_prime = False Then Return False
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, "7")
		      If is_prime = False Then Return False
		      is_prime = apmint_miller_rabin_pass(input, n_minus1, d, s, "61")
		      Return is_prime
		    End If
		  End If
		  
		  If passes <= 0 Then passes = 30
		  
		  // we have a larger prime; do multiple passes of miller-rabin
		  // for parainputter 'a'; need a number between 2 and n-1
		  var i64 As Int64
		  For iteration_count As Int64 = 1 To passes
		    i64 = Xojo.Math.RandomInt(2, CType(4759123140,Int64))
		    a = i64
		    If apmint_miller_rabin_pass(input, n_minus1, d, s, a) = False Then Return False
		  Next
		  
		  // number is likely prime
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_is_odd(input as apmint_module.apmint) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // return true if ampint is odd, false otherwise
		  
		  Return Not apmint_is_even(input)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_is_one(input as apmint_module.apmint) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // return true if ampint is = one
		  
		  If Input Is Nil Then Return False
		  
		  If Input.Sign > 0 And Input.used = 8 And Input.digits.UInt64Value(0) = &h1 Then
		    Return True
		  Else
		    Return False
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_is_power(input as apmint_module.apmint, byref base as apmint, byref power as apmint_module.apmint) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // This algorithm determines if an integer input can be expressed as an integer base raised to an integer power
		  //
		  // 1) determine if input = base ^ power, where power is a prime number
		  // 2) upper bound of power is ~log(input); testing will begin at upper bound and proceed down to 2
		  // 3) input will be tested by getting a base by calculating nth root of input and then raising the base to the power to check
		  //        to see if base ^ power = input
		  // 4) will return a boolean that's true if a base and power is found, false otherwise
		  // 5) the base and power will be returned in the arguments
		  
		  var is_it_a_power As Boolean
		  
		  If base Is Nil Then base = New apmint_module.apmint
		  If power Is Nil Then power = New apmint_module.apmint
		  
		  var input_abs As New apmint_module.apmint
		  apmint_abs (Input, input_abs) ' start with positive input, handle negative case at end
		  
		  If input_abs < 4 Then Return False
		  
		  // set upper bound for testing
		  var upper_bound As Int64 = apmint_get_bit_length(input_abs)
		  
		  var base_to_power As New apmint_module.apmint
		  
		  // test each prime from upper bound down to 2 by using a nth root to see if it has an integer base
		  For kk As UInt64 = upper_bound DownTo 2
		    apmint_from_uint64(kk, power)
		    If apmint_is_likely_prime(power) Then
		      apmint_nroot(input_abs, kk, base)
		      apmint_pow(base, power, base_to_power)
		      If base_to_power = input_abs Then
		        is_it_a_power = True
		        Exit
		      End If
		    End If
		  Next
		  
		  If Not is_it_a_power Then
		    apmint_from_uint64(&h0, base)
		    apmint_from_uint64(&h0, power)
		    Return is_it_a_power
		  End If
		  
		  // handle negative input now; power must be odd for negative input
		  If Input.Sign = -1 Then
		    If apmint_is_odd(power) Then
		      base = - base
		    Else
		      is_it_a_power = False
		      apmint_from_uint64(&h0, base)
		      apmint_from_uint64(&h0, power)
		    End If
		  End If
		  
		  Return is_it_a_power
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_is_square(input as apmint_module.apmint, byref square_root as apmint_module.apmint) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // this algorithm determines if an input is a perfect integer square
		  //   i.e., is input = sqrt(input) ^ 2
		  // returns true if input is perfect integer square; false otherwise
		  // returns square_root in argument if input is square
		  
		  var b1, b2 As apmint_module.apmint
		  var least_significant_digit, least_significant_digit2, input_mod_100, ii as Int64
		  var products(9) As Int32 = apmint_array_int32(0, 1, 4, 9, 6, 5, 6, 9, 4, 1)
		  var perfect_square_endings(21) As Int32 = apmint_array_int32(0, 1, 4, 9, 16, 21, 24, 25, 29, 36, 41, 44, 49, 56,  61, 64, 69, 76, 81, 84, 89, 96)
		  var square_possible As Boolean
		  
		  if square_root is nil then square_root = new apmint_module.apmint
		  
		  If input is nil or input.Sign = 0 Then   ' zero is perfect square
		    apmint_from_uint64(&h0, square_root)
		    Return True
		  End If
		  
		  If input.Sign = -1 Then Return False     ' negative number is not perfect square
		  
		  // get least significant digit of input
		  least_significant_digit = apmint_get_least_significant_digit(input)
		  
		  // compute 'input mod 100'
		  b1 = input Mod 100
		  input_mod_100 = apmint_to_int64(b1)
		  
		  // perfect square numbers only end in 22 possible endings, filter out others
		  For ii = 0 To 21
		    If input_mod_100 = perfect_square_endings(ii) Then
		      square_possible = True
		      Exit
		    End If
		  Next
		  If square_possible = False Then Return False
		  
		  // it's a possible square, let's take square root to see
		  apmint_sqrt(input, b1)
		  least_significant_digit2 = apmint_get_least_significant_digit(b1)
		  // check last digit and see if it can make last digit of input
		  If least_significant_digit <> products(least_significant_digit2) Then
		    Return False
		  End If
		  
		  b2 = b1 * b1
		  
		  If input = b2 Then
		    apmint_copy(b1, square_root)
		    Return True
		  Else
		    Return False
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_lcm(aa as apmint_module.apmint, bb as apmint_module.apmint, byref result as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // least common multiple of two apmints
		  
		  var a, b, gcd As apmint_module.apmint
		  
		  apmint_abs(aa, a)
		  apmint_abs(bb, b)
		  // result = (a * b) / gcd(a, b)
		  apmint_gcd(a, b, gcd)
		  apmint_multiply(a, b, result)
		  apmint_divide(result, gcd, result)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_max(paramarray apmint_array() as apmint_module.apmint) As apmint_module.apmint
		  // returns the maximum value of an array of apmint_s
		  
		  return apmint_max(apmint_array)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_max(apmint_array() as apmint_module.apmint) As apmint_module.apmint
		  // returns the maximum value of an array of apmint_s
		  
		  var result As apmint_module.apmint
		  var ii, jj as int64
		  
		  result = apmint_array(0)
		  jj = ubound(apmint_array)
		  
		  If jj = 0 Then
		    return result
		  Else
		    For ii = 1 To jj
		      If apmint_array(ii) > result Then
		        result = apmint_array(ii)
		      End If
		    Next
		  End If
		  
		  return result
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1, CompatibilityFlags = API2Only
		Protected Function apmint_miller_rabin_pass(n as apmint_module.apmint, n_minus1 as apmint_module.apmint, d as apmint_module.apmint, s as apmint_module.apmint, a as apmint_module.apmint) As Boolean
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // performs one Miller-Rabin prime number pimality test pass
		  // returns false if input is definitely not prime (composite) and true if n is possibly prime
		  
		  var m As apmint_module.apmint
		  
		  var b1 As New apmint_module.apmint
		  m = apmint_mod_exp(a, d, n)
		  If m = 1 Or m = n_minus1 Then Return True
		  While b1 < s
		    m = (m * m) Mod n
		    If m = 1 Then Return False
		    If m = n_minus1 Then Return True
		    b1 = b1 + 1
		  Wend
		  
		  Return False
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_min(paramarray apmint_array() as apmint_module.apmint) As apmint_module.apmint
		  // returns the minimum value of an array of apmint_s
		  
		  return apmint_min(apmint_array)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_min(apmint_array() as apmint_module.apmint) As apmint_module.apmint
		  // returns the minimum value of an array of apmint_s
		  
		  var result As apmint_module.apmint
		  var ii, jj as int64
		  
		  result = apmint_array(0)
		  jj = ubound(apmint_array)
		  
		  If jj = 0 Then
		    return result
		  Else
		    For ii = 1 To jj
		      If apmint_array(ii) < result Then
		        result = apmint_array(ii)
		      End If
		    Next
		  End If
		  
		  return result
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_modulo(input as apmint_module.apmint, modulus as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = input modulo modulus (output is overwritten)
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  var input_sign As Int64 = Input.Sign
		  var modulus_sign As Int64 = modulus.Sign
		  
		  If Input_Sign * modulus_Sign = 0 Then
		    If Input_Sign = 0 Then
		      output.Sign = 0
		      output.used = 0
		      Return
		    Else ' modulus = 0 is undefined
		      var re As New RuntimeException
		      re.Message = "apmint_DivideByZeroErr"
		      Raise re
		    End If
		  End If
		  
		  If Input_Sign * modulus_Sign = 1 Then
		    
		    // input and modulus sign the same
		    apmint_remainder(Input, modulus, output)
		    If output.Sign <> 0 Then output.Sign = modulus_sign
		    
		  Else
		    
		    // input and modulus has opposite signs
		    var input_abs, modulus_abs As apmint_module.apmint
		    
		    If Input_Sign = -1 Then
		      apmint_abs(Input, input_abs)
		    Else
		      input_abs = Input
		    End If
		    
		    If modulus_Sign = -1 Or modulus Is output Then
		      apmint_abs(modulus, modulus_abs)
		    Else
		      modulus_abs = modulus
		    End If
		    
		    apmint_remainder(input_abs, modulus_abs, output)
		    
		    If output.Sign <> 0 Then
		      apmint_subtract(output, modulus_abs, output)
		      output.Sign = modulus_Sign
		    End If
		    
		  End If
		  
		  Return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function apmint_mod_exp(a as apmint_module.apmint, b as apmint_module.apmint, c as apmint_module.apmint) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking false
		  #pragma NilObjectChecking false
		  
		  // modular exponentiation of apmint
		  // a ^ b mod c
		  
		  If c = 1 Or c = -1 Then Return New apmint_module.apmint  ' mod 1 is 0
		  
		  If c.Sign = 0 Then                                       ' mod 0 is undefined
		    var re As New RuntimeException
		    re.Message = "apmint_DivideByZeroErr"
		    Raise re
		  End If
		  
		  var result As apmint_module.apmint = 1
		  var ii As Int64
		  
		  For ii = apmint_get_bit_length(b) DownTo 0
		    result = (result * result) Mod c
		    If apmint_get_bit(b,ii) <> 0 Then
		      result = (result * a) Mod c
		    End If
		  Next
		  
		  Return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_multiply(num1 as apmint_module.apmint, num2 as apmint_module.apmint, byref result as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // result = num1 * num2 (overwrites result)
		  
		  Static output As apmint_module.apmint = 0
		  
		  If result Is Nil Then result = New apmint_module.apmint
		  
		  If num1.Sign * num2.Sign = 0 Then
		    result.Sign = 0
		    result.used = 0
		    Return
		  End If
		  
		  // 'a' will be shorter of the two integers, 'b' will be the longer, asize and bsize are lengths of the two respectively
		  var a, b As apmint_module.apmint
		  If num1.used < num2.used Then
		    a = num1
		    b = num2
		  Else
		    a = num2
		    b = num1
		  End If
		  
		  var asize As Int64 = a.used
		  var bsize As Int64 = b.used
		  
		  #If limit_bytes Then
		    If asize + bsize > apmint_module.max_bytes Then
		      var re As New RuntimeException
		      re.Message = "apmint_UnderOverflowErr"
		      Raise re
		    End If
		  #EndIf
		  
		  //multiply using FFTs for very large input
		  If asize > 800 And bsize > 800 Then
		     result = apmint_fft_multiply(num1, num2)
		    Return
		  End If
		  
		  // multiply using Karatusba algorithm for larger numbers
		  If asize > 400 And bsize > 400 Then
		    
		    var split_point As Int64 = (bsize \ 8 + 1) \ 2
		    var nwords As Int64 = split_point * 2
		    var a1 As New apmint_module.apmint
		    var a2 As New apmint_module.apmint
		    var b1 As New apmint_module.apmint
		    var b2 As New apmint_module.apmint
		    
		    //split b  (b1 is most significant half, b2 is least significant half)
		    var temp As Int64 = split_point * 8
		    b1.digits = b.digits.Midb(temp, bsize - temp)
		    b1.used = b1.digits.size
		    b2.digits = b.digits.Leftb(temp)
		    b2.used = b2.digits.size
		    b1.Sign = 1
		    b1.normalize
		    b2.Sign = 1
		    b2.normalize
		    
		    //split a   (a1 is most significant half, a2 is least significant half)
		    If asize \ 8 > split_point Then
		      a1.digits = a.digits.Midb(temp, asize - temp)
		      a1.used = a1.digits.size
		      a2.digits = a.digits.Leftb(temp)
		      a2.used = a2.digits.size
		      a1.Sign = 1
		      a1.normalize
		      a2.Sign = 1
		      a2.normalize
		    Else  // leave a1 zero, copy a to a2
		      apmint_abs(a, a2)
		    End If
		    
		    var P1, P2, P3 As apmint_module.apmint
		    
		    // multiply using Karatusba algorithm
		    
		    P1 = a1 * b1                              ' possible recursive multiplies here
		    P2 = a2 * b2                              '   and here
		    P3 = (a1 + a2) * (b1 + b2) - P1 - P2      '  and here as well
		    
		    If P1.Sign <> 0 Then apmint_shift_left(P1, nwords * CType(64, Int64), P1)     ' shift P1 left by nwords words
		    If P3.Sign <> 0 Then apmint_shift_left(P3, split_point * CType(64,Int64), P3) ' shift P3 left by split_point words
		    
		    result = P1 + P2 + P3
		    result.Sign = num1.Sign * num2.Sign
		    Return
		    
		  End If
		  
		  // use school multiply algorithm
		  output.Sign = num1.Sign * num2.Sign
		  output.used = asize + bsize
		  If output.digits Is Nil Or output.digits.size < output.used Then apmint_grow(output, output.used)
		  var aptr As ptr = a.digits
		  var bptr As ptr = b.digits
		  var optr As ptr = output.digits
		  var ap, bp, op As Int64
		  var carry, ui1 As UInt64
		  
		  // multiply using 32 bit words
		  
		  // multiply by first element of 'a'
		  While True
		    op = bp
		    ui1 = CType(aptr.UInt32(ap), UInt64) * CType(bptr.UInt32(bp), UInt64) + carry
		    optr.UInt32(op) = ui1 And &hFFFFFFFF
		    carry = ui1 \ &h100000000
		    bp = bp + 4
		    If bp >= bsize Then
		      optr.UInt32(op + 4) =  carry
		      carry = 0
		      bp = 0
		      ap = ap + 4
		      If ap >= asize Then
		        output.normalize
		        apmint_copy(output, result)
		        Return
		      End If
		      Exit
		    End If
		  Wend
		  
		  // multiply by remaining elements of 'a'
		  While True
		    op = ap + bp
		    ui1 = CType(optr.UInt32(op), UInt64) + CType(aptr.UInt32(ap), UInt64) * CType(bptr.UInt32(bp), UInt64) + carry
		    optr.UInt32(op) = ui1 And &hFFFFFFFF
		    carry = ui1 \ &h100000000
		    bp = bp + 4
		    If bp >= bsize Then
		      optr.UInt32(op + 4) =  carry
		      carry = 0
		      bp = 0
		      ap = ap + 4
		      If ap >= asize Then
		        output.normalize
		        apmint_copy(output, result)
		        Return
		      End If
		    End If
		  Wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_multiply_by2(num as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = num * 2 (shift left by one), output is overwritten
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If num.Sign = 0 Then
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  var num_used As Int64 = num.used
		  var output_size As Int64 = num_used + 8
		  output.Sign = num.Sign
		  
		  #If limit_bytes Then
		    If output_size > apmint_module.max_bytes Then
		      var re As New RuntimeException
		      re.Message = "apmint_UnderOverflowErr"
		      Raise re
		    End If
		  #EndIf
		  
		  If output.used < output_size Then apmint_grow(output, output_size)
		  num.used = num_used  ' num could be input and output
		  
		  var offset As Int64
		  var rptr As ptr = output.digits
		  var nptr As ptr = num.digits
		  var upper_word As Int64 = num_used - 8
		  var ui1, upper_bit As UInt64
		  
		  While True
		    ui1 = nptr.UInt64(offset)
		    rptr.UInt64(offset) = (ui1 * &h2) Or upper_bit
		    upper_bit = ui1 \ &h8000000000000000
		    If offset >= upper_word Then Exit
		    offset = offset + 8
		  Wend
		  
		  If upper_bit <> &h0 Then
		    rptr.UInt64(offset + 8) = upper_bit
		    output.used = output_size
		  Else
		    output.used = num_used
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_negate(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // output = -input
		  
		  If Input Is output Then
		    output.Sign = -output.Sign
		    Return
		  Else
		    apmint_copy(Input, output)
		    output.Sign = -output.Sign
		    Return
		  End If
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_next_prime(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns a prime number that is > input
		  
		  Static one As apmint_module.apmint = 1
		  Static two As apmint_module.apmint = 2
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If Input Is Nil Or Input < two Then
		    apmint_copy(two, output)
		    Return
		  End If
		  
		  apmint_add(Input, one, output)
		  
		  While True
		    If apmint_is_likely_prime(output) Then Exit
		    apmint_add(output, one , output)
		  Wend
		  
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_nroot(input as apmint_module.apmint, nth as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // nth root of apmint, output = 'nth' root of 'input'
		  // uses the newton iteration for nth root
		  // valid for nth >= 1
		  
		  var n, last_x, big_nth, t1 As apmint_module.apmint
		  var nth_minus1 As Int64
		  Static one as apmint_module.apmint = 1
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If Input Is Nil Or Input.Sign = 0 Or nth = 0 Then  ' nroot(0) = 0
		    apmint_from_uint64(&h0, output)
		    Return
		  End If
		  
		  If apmint_is_one(input) Then                       ' nroot(1) = 1
		    apmint_from_uint64(&h1, output)
		    Return
		  End If
		  
		  If (nth Mod 2 = 0) And input.Sign = -1 Then        ' input can't be negative if nth even
		    var re As New RuntimeException
		    re.Message = "apmint_NegativeArgumentErr"
		    Raise re
		  End If
		  
		  If nth = 1 Then                                    ' if nth is one, result is input
		    apmint_copy(input, output)
		    return
		  end if
		  
		  apmint_abs(input, n)
		  var bit_count As Int64 = apmint_get_bit_length(n)
		  big_nth = nth
		  
		  If apmint_get_bit_length(big_nth) > bit_count Then  ' catch high nth value
		    If input.Sign = 1 Then
		      apmint_from_uint64(&h1, output)
		      Return
		    Else
		      apmint_from_int64(-1, output)
		      Return
		    End If
		  End If
		  
		  // rough guess to start iteration
		  apmint_shift_right(n, bit_count - (bit_count \ nth), output)
		  apmint_add(output, one, output)
		  
		  If output < 2 Then apmint_from_uint64(&h2, output)   ' set min value for guess
		  nth_minus1 = nth - 1
		  big_nth = nth
		  
		  Do ' do newton iteration
		    apmint_copy(output, last_x)
		    //output = last_x + ((n \ last_x.Pow(nth_minus1)) - last_x) \ big_nth
		    apmint_pow(last_x, nth_minus1, output)
		    apmint_divide(n, output, output)
		    apmint_subtract(output, last_x, output)
		    apmint_divide(output, big_nth, output)
		    apmint_add(output, last_x, output)
		    
		  Loop Until last_x = output
		  
		  While True
		    apmint_pow(output, big_nth, t1) ' while output ^ big_nth > n
		    If t1 <= n Then Exit
		    apmint_subtract(output, one, output)
		  Wend
		  
		  output.Sign = input.Sign
		  Return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_pow(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns output = input1 ^ input2, output is overwritten
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  var base As New apmint_module.apmint
		  var exp As New apmint_module.apmint
		  apmint_copy(input1, base)
		  apmint_copy(input2, exp)
		  
		  If base.sign = 0 Then            ' 0 raised to power is zero
		    apmint_from_uint64(0, output)
		    Return
		  End If
		  
		  If exp.sign = 0 Then             ' number raised to zero power is one
		    apmint_from_uint64(1, output)
		    Return
		  Elseif apmint_is_one(exp) Then   ' number raised to one is number
		    apmint_copy(base, output)
		    Return
		  End If
		  
		  If apmint_is_one(base) Then
		    apmint_from_uint64(1, output)   ' 1 raised to any power is one
		    Return
		  Elseif base = -1 Then
		    If apmint_is_even(exp) Then
		      apmint_from_uint64(1, output)  ' -1 raised to even power is 1
		      Return
		    Else
		      apmint_from_int64(-1, output)  ' -1 raised to odd power is -1
		      Return
		    End If
		  End If
		  
		  If exp.Sign = -1 Then             ' fraction, return 0
		    apmint_from_uint64(0, output)
		    Return
		  End If
		  
		  apmint_from_uint64(1, output)
		  
		  While exp.Sign > 0
		    
		    If apmint_is_odd(exp) Then
		      output = output * base
		    End If
		    
		    base = base * base
		    apmint_divide_by2(exp, exp)
		    
		  Wend
		  
		  Return
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_previous_prime(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns a prime number that is > input
		  
		  Static one as apmint_module.apmint = 1
		  Static two as apmint_module.apmint = 2
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If Input Is Nil Or Input <= two Then
		    apmint_from_uint64(&h2, output)
		    Return
		  End If
		  
		  apmint_subtract(Input, one, output)
		  
		  While True
		    If apmint_is_likely_prime(output) Then Exit
		    apmint_subtract(output, one , output)
		  Wend
		  
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_prime_sieve(max_prime as uint64) As int64()
		  // sieves prime numbers up to 'max_prime' argument
		  //  returns an int64 array with the primes
		  //  holds the last primes sieved in case the same or less are requested again
		  //   if more are requested a new sieve is run
		  
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  var prime_array() As Int64           ' array of primes to return
		  Static upper_prime As Int64          ' max prime currently in storage
		  Static primes() As Byte              ' array to store primes (used as array of bits)
		  var masks() As UInt32 = apmint_array_uint32(&h01,&h02,&h04,&h08,&h10,&h20,&h40,&h80)  ' bit masks
		  var prime_count As Int64
		  var prime As Int64 = 2
		  var array_index, ii, jj As Int64
		  
		  If max_prime < 2 Then max_prime = 2
		  
		  // if we have previously sieved enough primes, return a new array with just the
		  //  ones requested
		  If max_prime <= upper_prime Then
		    
		    // count the number primes to return
		    For ii = 2 To max_prime
		      If (primes(ii \ 8) And masks(ii Mod 8)) = masks(ii Mod 8) Then
		        prime_count = prime_count + 1
		      End If
		    Next
		    
		    // size array to hold just the primes to return
		    redim prime_array(prime_count - 1)
		    
		    // move primes into array to return
		    For ii = 2 To max_prime
		      If (primes(ii \ 8) And masks(ii Mod 8)) = masks(ii Mod 8) Then
		        prime_array(array_index) = ii
		        array_index = array_index + 1
		      End If
		    Next
		    Return prime_array
		  End If
		  
		  // not enough primes, sieve
		  
		  //  resize bit array
		  redim primes((max_prime + 7) \ 8)     ' create bit array (bits will be set for prime numbers)
		  For ii = 0 To ubound(primes)          ' default to all numbers prime
		    primes(ii) = &hff
		  Next
		  primes(0) = primes(0) And &hfc        ' 0 and 1 not prime
		  
		  // sieve the primes
		  While prime <= max_prime \ 2          ' eliminate numbers that are multiples of primes
		    If (primes(prime \ 8) And masks(prime Mod 8)) = masks(prime Mod 8)Then
		      jj = prime * 2
		      While jj <= max_prime
		        primes(jj \ 8) = primes(jj \ 8)  And (masks(jj Mod 8) Xor &hff)
		        jj = jj + prime
		      Wend
		    End If
		    prime = prime + 1
		  Wend
		  
		  upper_prime = max_prime                 ' remember how high we have sieved
		  
		  // count the primes sieved
		  For ii = 2 To max_prime
		    If (primes(ii \ 8) And masks(ii Mod 8)) = masks(ii Mod 8) Then
		      prime_count = prime_count + 1
		    End If
		  Next
		  
		  // size array to hold just the primes to return
		  redim prime_array(prime_count - 1)
		  
		  upper_prime = max_prime                  ' remember how high we have sieved
		  
		  // move primes into array to return
		  For ii = 2 To max_prime
		    If (primes(ii \ 8) And masks(ii Mod 8)) = masks(ii Mod 8) Then
		      prime_array(array_index) = ii
		      array_index = array_index + 1
		    End If
		  Next
		  
		  Return prime_array
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_random(optional num_bits as uint64 = 16, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // Generate random number with number of bits specified in argument
		  
		  if output is nil then output = new apmint_module.apmint
		  
		  If num_bits = 0 Then num_bits = 16
		  
		  var ii, jj, kk As Int64
		  
		  kk = num_bits - 1
		  For ii = 0 To kk
		    jj = RandomInt(0, 1)
		    If jj = 1 Then
		      apmint_bit_set(output, ii, output)
		    End If
		  Next
		  
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_random_inrange(x as apmint_module.apmint, y as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // Generates a random integer that is in the range (inclusive) of arguments 'x' and 'y'
		  
		  var a, b, random_input, b1, t1 As apmint_module.apmint
		  var bit_size As Int64
		  Static one As apmint_module.apmint = 1
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  a = apmint_copy(x)
		  b = apmint_copy(y)
		  
		  If a = b Then
		    apmint_copy(a, output)
		    Return
		  End If
		  
		  If a < b Then apmint_swap(a, b)
		  
		  apmint_subtract(b, a, t1)
		  bit_size = apmint_get_bit_length(t1)
		  apmint_from_uint64(&h1, b1)
		  //b1 = b1.shift_left(bit_size) - 1
		  apmint_shift_left(b1, bit_size, b1)
		  apmint_subtract(b1, one, b1)
		  apmint_random(bit_size, random_input)
		  //output = (random_input * (a - b) \ b1) + b
		  apmint_subtract(a, b, t1)
		  apmint_multiply(t1, random_input, output)
		  apmint_divide(output, b1, output)
		  apmint_add(output, b, output)
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_remainder(dividend_in as apmint_module.apmint, divisor_in as apmint_module.apmint,  byref remainder as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // divide ampint by apmint returning just remainder (remainder is overwritten)
		  
		  Static base_32bit As UInt64 = &h100000000    ' 32 bit base
		  Static lo_mask_32bit As UInt64 = &hFFFFFFFF  ' 32-bit lo mask
		  Static U64_Zero As UInt64 = CType(0, UInt64)
		  Static U64_One As UInt64 = CType(1, UInt64)
		  Static dividend_working As New memoryblock(800)
		  Static divisor_working As New memoryblock(800)
		  Static div_prod As New memoryblock(800)
		  
		  If remainder Is Nil Then remainder = New apmint_module.apmint
		  
		  // below needed because input could be overwritten
		  var dividend_in_sign As Int64 = dividend_in.Sign
		  var divisor_in_sign As Int64 = divisor_in.Sign
		  
		  // test for special case of dividend = 0
		  If dividend_in_sign = 0 Then
		    apmint_from_uint64(&h0, remainder)
		    Return
		  End If
		  
		  // if divide by zero, raise exception
		  If divisor_in_sign = 0 Then
		    var re As New RuntimeException
		    re.Message = "apmint_DivideByZeroErr"
		    Raise re
		  End If
		  
		  // test for special case of both values of Uint64 size
		  If divisor_in.used + dividend_in.used = 16 Then
		    var uquotient As UInt64 = dividend_in.digits.Uint64value(0) \ divisor_in.digits.UInt64Value(0)
		    apmint_from_uint64(dividend_in.digits.UInt64Value(0) - uquotient * divisor_in.digits.Uint64value(0), remainder)
		    If remainder.Sign <> 0 Then remainder.Sign = dividend_in_sign
		    Return
		  End If
		  
		  // test for special case of dividend <= to divisor
		  var icmp As Int64 = apmint_compare_abs(dividend_in, divisor_in)
		  If icmp = -1 Then
		    apmint_copy(dividend_in, remainder)
		    Return
		  Elseif icmp = 0 Then
		    apmint_from_uint64(&h0, remainder)
		    Return
		  End If
		  
		  // remove trailing zero from dividend and divisor
		  var dividend_digits_size As Int64 = dividend_in.used
		  If dividend_in.digits.Int32Value(dividend_digits_size - 4) = 0 Then dividend_digits_size = dividend_digits_size - 4
		  var divisor_digits_size As Int64 = divisor_in.used
		  If divisor_in.digits.Int32Value(divisor_digits_size - 4) = 0 Then divisor_digits_size = divisor_digits_size - 4
		  
		  var dividend_ptr As ptr = dividend_in.digits
		  var divisor_ptr As ptr = divisor_in.digits
		  var ii, jj, kk As Int64
		  
		  //// treat case of single digit divisor as special case
		  If divisor_digits_size = 4 Then
		    var quotienti64, remainderi64, divisori64 As UInt64
		    If dividend_digits_size = 4 Then
		      // one divisor digit, one dividend digit
		      quotienti64 = CType(dividend_ptr.UInt32(0), UInt64) \ CType(divisor_ptr.UInt32(0), UInt64)
		      remainderi64 = CType(dividend_ptr.UInt32(0), UInt64) - (quotienti64 * CType(divisor_ptr.UInt32(0), UInt64))
		      apmint_from_int64(remainderi64, remainder)
		      If remainder.Sign <> 0 Then remainder.Sign = dividend_in_sign
		      Return
		    Else
		      // one divisor digit, two or more dividend digits
		      var dp As Int64 = dividend_digits_size - 4  ' pointer to last digit of dividend
		      var rp As Int64
		      divisori64 = divisor_ptr.UInt32(0)
		      
		      While dp >= 0
		        remainderi64 = (remainderi64 * base_32bit) + CType(dividend_ptr.UInt32(dp), UInt64)
		        quotienti64 = remainderi64 \ divisori64
		        remainderi64 = remainderi64 - (quotienti64 * divisori64)
		        rp = rp + 1
		        dp = dp - 4
		      Wend
		      apmint_from_int64(remainderi64, remainder)
		      If remainder.Sign <> 0 Then remainder.Sign = dividend_in_sign
		      
		      Return
		      
		    End If
		  End If
		  //// end of single digit divisor case
		  
		  // copy dividend and divisor to working areas; divisor will be padded by a zero in front; msd first
		  var dividend_working_size As Int64 = dividend_digits_size * 2 + 8
		  If dividend_working.size < dividend_working_size Then
		    dividend_working = New memoryblock(dividend_working_size)
		  End If
		  var dividend_working_ptr As ptr = dividend_working
		  var ptr1, ptr2 As Int64
		  ptr1 = 8
		  ptr2 = dividend_digits_size - 4
		  While ptr2 >= 0
		    dividend_working_ptr.UInt32(ptr1) = dividend_ptr.UInt32(ptr2)
		    ptr1 = ptr1 + 8
		    ptr2 = ptr2 - 4
		  Wend
		  
		  var divisor_working_size As Int64 = divisor_digits_size * 2
		  If divisor_working.size < divisor_working_size Then
		    divisor_working = New memoryblock(divisor_working_size)
		  End If
		  var divisor_working_ptr As ptr = divisor_working
		  ptr1 = 0
		  ptr2 = divisor_digits_size - 4
		  While ptr2 >=0
		    divisor_working_ptr.UInt32(ptr1) = divisor_ptr.UInt32(ptr2)
		    ptr1 = ptr1 + 8
		    ptr2 = ptr2 - 4
		  Wend
		  
		  // normalize divisor and dividend to improve division guesses; must divide out of final remainder at end
		  var normalization_factor As UInt64 = base_32bit \ (divisor_working_ptr.UInt64(0) + U64_One)
		  var divisor_length As Int64  = divisor_working_size \ 8
		  var dividend_length As Int64 = dividend_working_size \ 8
		  var ui1, ui2, carry As UInt64
		  
		  If (normalization_factor <> &h1) Then
		    
		    // Multiply divisor and dividend by normalization factor
		    For ii= (divisor_length-1) * 8 DownTo 0 Step 8
		      ui1 = divisor_working_ptr.UInt64(ii)
		      ui1 = (ui1 * normalization_factor) + carry
		      carry = ui1 \ base_32bit
		      divisor_working_ptr.UInt64(ii) = ui1 And lo_mask_32bit
		    Next
		    
		    carry = 0
		    For ii= (dividend_length-1) * 8 DownTo 0 Step 8
		      ui1 = dividend_working_ptr.UInt64(ii)
		      ui1 = (ui1 * normalization_factor) + carry
		      carry = ui1 \ base_32bit
		      dividend_working_ptr.UInt64(ii) = ui1 And lo_mask_32bit
		    Next
		    
		  End If
		  
		  var divisor1d As UInt64 = divisor_working_ptr.UInt64(0)               ' divisor first word used by division guesses
		  var divisions_remaining As Int64 = dividend_length - divisor_length   ' how many divisions left
		  var dividend2d As UInt64                                              ' will hold two dividend digits
		  var division_guess As UInt64                                          ' a guess at a quotient digit
		  var guess_good As Boolean                                             ' was our quess good
		  var borrow As Boolean                                                 ' detect borrows in subtraction
		  var dividend_offset As Int64                                          ' increased when dividend is shifted
		  If div_prod.size < dividend_working_size Then
		    div_prod = New memoryblock(dividend_working_size)   ' temp working area used to check division
		  End If
		  var div_prod_ptr As ptr = div_prod
		  
		  While (True)
		    
		    // Get the two digits from working remainder for quess
		    dividend2d = dividend_working_ptr.UInt64(dividend_offset) * base_32bit + dividend_working_ptr.UInt64(dividend_offset + 8)
		    
		    division_guess = dividend2d \ divisor1d  ' guess the next quotient digit
		    If division_guess > lo_mask_32bit Then division_guess = lo_mask_32bit
		    
		    If (division_guess <> 0) Then
		      
		      // multiply divisor by guess
		      carry = 0
		      For ii = (divisor_length-1) * 8 DownTo 0 Step 8
		        jj = ii + 8
		        ui1 = (divisor_working_ptr.UInt64(ii) * division_guess) + carry
		        carry = ui1 \ base_32bit
		        div_prod_ptr.UInt64(jj) = ui1 And lo_mask_32bit
		      Next
		      div_prod_ptr.UInt64(0) = carry
		      
		      // check to see if guess is correct, if not subtract one from guess and try again (and again)
		      While True
		        guess_good = True
		        jj = divisor_length * 8
		        For ii = 0 To jj Step 8
		          kk = ii + dividend_offset
		          If dividend_working_ptr.UInt64(kk) <>  div_prod_ptr.UInt64(ii) Then
		            If dividend_working_ptr.UInt64(kk) < div_prod_ptr.UInt64(ii) Then
		              guess_good = False
		              Exit
		            Else
		              Exit
		            End If
		          End If
		        Next
		        
		        If  guess_good = True Then Exit
		        division_guess = division_guess - 1      ' bad guesses are almost always off by one
		        
		        // multiply divisor by guess again
		        carry = 0
		        For ii = (divisor_length-1) * 8 DownTo 0 Step 8
		          jj = ii + 8
		          ui1 = (divisor_working_ptr.UInt64(ii) * division_guess) + carry
		          carry = ui1 \ base_32bit
		          div_prod_ptr.UInt64(jj) = ui1 And lo_mask_32bit
		        Next
		        div_prod_ptr.UInt64(0) = carry
		        
		      Wend
		      
		      // subtract divisor * guess from working remainder (dividend_working) for new working remainder
		      borrow = False
		      jj = divisor_length * 8
		      For ii = jj DownTo 0 Step 8
		        kk = ii + dividend_offset
		        If borrow Then
		          If (dividend_working_ptr.UInt64(kk) = U64_Zero) Then
		            dividend_working_ptr.UInt64(kk) = lo_mask_32bit
		          Else
		            dividend_working_ptr.UInt64(kk) = dividend_working_ptr.UInt64(kk) - U64_One
		            borrow = False
		          End If
		        End If
		        
		        dividend_working_ptr.Int64(kk) = dividend_working_ptr.Int64(kk) - div_prod_ptr.Int64(ii)
		        If (dividend_working_ptr.Int64(kk) < 0) Then
		          dividend_working_ptr.Int64(kk) = dividend_working_ptr.Int64(kk) + base_32bit
		          borrow = True
		        End If
		      Next
		    End If
		    
		    // if the required number of divisions have been done, then exit loop
		    divisions_remaining = divisions_remaining - 1
		    If divisions_remaining = 0 Then Exit While
		    
		    // shift dividend to the left
		    dividend_offset = dividend_offset + 8
		  Wend
		  
		  // dividend_working is now final remainder (divide out normalization_factor)
		  If normalization_factor <> &h1 Then
		    ui1 = 0
		    ii = divisor_length * 8 + dividend_offset
		    For jj = dividend_offset To ii Step 8
		      ui1 = (ui1 * base_32bit) + dividend_working_ptr.UInt64(jj)
		      ui2 = ui1 \ normalization_factor
		      ui1 = ui1 - (ui2 * normalization_factor)
		      dividend_working_ptr.UInt64(jj) = ui2
		    Next
		  End If
		  
		  // now place remainder in the bigints (words reversed)
		  
		  var first_remainder_digit As Int64 = -1
		  ii = divisor_length * 8 + dividend_offset
		  For jj = dividend_offset To ii Step 8
		    If dividend_working_ptr.UInt64(jj) <> 0 Then
		      first_remainder_digit = jj
		      Exit
		    End If
		  Next
		  
		  If first_remainder_digit <> -1 Then
		    ptr1 = 0
		    
		    var remainder_length As Int64 = (ii - first_remainder_digit) \ 2 + 4
		    If remainder_length Mod 8 <> 0 Then remainder_length = remainder_length + 4
		    If remainder_length > remainder.used Then apmint_grow(remainder, remainder_length)
		    remainder.used = remainder_length
		    var remainder_ptr As ptr = remainder.digits
		    For jj = ii DownTo first_remainder_digit Step 8
		      remainder_ptr.UInt32(ptr1) = dividend_working_ptr.UInt64(jj)
		      ptr1 = ptr1 + 4
		    Next
		    While ptr1 < remainder.used     ' clear remaining used space
		      remainder_ptr.UInt32(ptr1) = 0
		      ptr1 = ptr1 + 4
		    Wend
		    remainder.Sign = dividend_in_sign
		  End If
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_shift_bits_left(byref input as apmint_module.apmint, bit_shift as int64)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // shifts apmint 'input' left by 'bit_shift' bits
		  // valid bit shift range is 1-63
		  
		  Static pow2() As UInt64 = Array(&h1, &h2, &h4, &h8, &h10, &h20, &h40, &h80, _
		  &h100,             &h200,             &h400,             &h800,             &h1000,             &h2000,             &h4000,             &h8000, _
		  &h10000,           &h20000,           &h40000,           &h80000,           &h100000,           &h200000,           &h400000,           &h800000, _
		  &h1000000,         &h2000000,         &h4000000,         &h8000000,         &h10000000,         &h20000000,         &h40000000,         &h80000000, _
		  &h100000000,       &h200000000,       &h400000000,       &h800000000,       &h1000000000,       &h2000000000,       &h4000000000,       &h8000000000, _
		  &h10000000000,     &h20000000000,     &h40000000000,     &h80000000000,     &h100000000000,     &h200000000000,     &h400000000000,     &h800000000000, _
		  &h1000000000000,   &h2000000000000,   &h4000000000000,   &h8000000000000,   &h10000000000000,   &h20000000000000,   &h40000000000000,   &h80000000000000, _
		  &h100000000000000, &h200000000000000, &h400000000000000, &h800000000000000, &h1000000000000000, &h2000000000000000, &h4000000000000000, &h8000000000000000)
		  
		  
		  If input Is Nil Or input.Sign = 0 Or bit_shift <= 0 Or bit_shift > 63 Then Return
		  
		  var rptr As ptr = input.digits
		  var right_side_bits, itemp As UInt64
		  var pow2_1 As UInt64 = pow2(bit_shift)
		  var pow2_2 As UInt64 = pow2(64 - bit_shift)
		  var upper As Int64 = input.used - 8
		  var rp As Int64
		  
		  While True
		    itemp = rptr.UInt64(rp)
		    rptr.UInt64(rp) = (itemp * pow2_1) Or right_side_bits  ' word shifted left or'd with right side of previous word
		    right_side_bits = itemp \ pow2_2                       ' right side is used with next word
		    If rp = upper Then Exit
		    rp = rp + 8
		  Wend
		  
		  If right_side_bits <> 0 Then ' shift will create new word?
		    var result_size As Int64 = input.used + 8
		    #If limit_bytes Then
		      If result_size > apmint_module.max_bytes Then
		        var re As New RuntimeException
		        re.Message = "apmint_UnderOverflowErr"
		        Raise re
		      End If
		    #EndIf
		    input.used = result_size
		    input.digits.size = result_size
		    input.digits.UInt64Value(result_size - 8) = right_side_bits
		  End If
		  
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_shift_bits_right(byref input as apmint_module.apmint, bit_shift as int64)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // shifts apmint 'input' right by 'bit_shift' bits
		  // valid bit shift range is 1-63
		  
		  Static pow2() As UInt64 = Array(&h1, &h2, &h4, &h8, &h10, &h20, &h40, &h80, _
		  &h100,             &h200,             &h400,             &h800,             &h1000,             &h2000,             &h4000,             &h8000, _
		  &h10000,           &h20000,           &h40000,           &h80000,           &h100000,           &h200000,           &h400000,           &h800000, _
		  &h1000000,         &h2000000,         &h4000000,         &h8000000,         &h10000000,         &h20000000,         &h40000000,         &h80000000, _
		  &h100000000,       &h200000000,       &h400000000,       &h800000000,       &h1000000000,       &h2000000000,       &h4000000000,       &h8000000000, _
		  &h10000000000,     &h20000000000,     &h40000000000,     &h80000000000,     &h100000000000,     &h200000000000,     &h400000000000,     &h800000000000, _
		  &h1000000000000,   &h2000000000000,   &h4000000000000,   &h8000000000000,   &h10000000000000,   &h20000000000000,   &h40000000000000,   &h80000000000000, _
		  &h100000000000000, &h200000000000000, &h400000000000000, &h800000000000000, &h1000000000000000, &h2000000000000000, &h4000000000000000, &h8000000000000000)
		  
		  
		  If input is nil or input.Sign = 0 or bit_shift <= 0 or bit_shift > 63 Then return
		  
		  var rptr As ptr = input.digits
		  var rp As Int64 =  input.used - 8
		  var left_side_bits, itemp As UInt64
		  var pow2_1 As UInt64 = pow2(bit_shift)
		  var pow2_2 As UInt64 = pow2(64 - bit_shift)
		  
		  While rp >= 0
		    itemp = rptr.UInt64(rp)
		    rptr.UInt64(rp) = (itemp \ pow2_1) Or left_side_bits
		    left_side_bits = itemp * pow2_2
		    rp = rp - 8
		  Wend
		  
		  input.normalize
		  
		  Return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_shift_left(input as apmint_module.apmint, bit_shift as int64) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns 'input' shifted left 'bit_shift' bits
		  
		  var result As New apmint_module.apmint
		  apmint_shift_left(input, bit_shift, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_shift_left(input as apmint_module.apmint, bit_shift as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = input shifted left 'bit_shift' bits (output is overwritten)
		  apmint_shift_words_left(input, bit_shift \ 64, output) ' whole word shift
		  apmint_shift_bits_left(output, bit_shift Mod 64)       ' left over bit shift
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_shift_right(input as apmint_module.apmint, bit_shift as int64) As apmint_module.apmint
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // returns 'input' shifted left 'bit_shift' bits
		  
		  var result As New apmint_module.apmint
		  apmint_shift_right(input, bit_shift, result)
		  Return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_shift_right(input as apmint_module.apmint, bit_shift as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = 'input' shifted right by 'bit_shift' bits (output is overwritten)
		  
		  apmint_shift_words_right(input, bit_shift \ 64, output) ' whole word shift
		  apmint_shift_bits_right(output, bit_shift Mod 64)     ' left over bit shift
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_shift_words_left(input as apmint_module.apmint, word_shift as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = input shifted left 'word_shift' words (output is overwritten)
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If input.Sign = 0 Then
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  var byte_shift As Int64 = word_shift * 8
		  var output_size As Int64 = input.used + byte_shift
		  
		  #If limit_bytes Then
		    If output_size > apmint_module.max_bytes Then
		      var re As New RuntimeException
		      re.Message = "apmint_UnderOverflowErr"
		      Raise re
		    End If
		  #EndIf
		  
		  // copy input to output with shift
		  Var new_mb As New MemoryBlock(output_size)
		  new_mb.StringValue(byte_shift, output_size - byte_shift) = input.digits.StringValue(0, output_size - byte_shift)
		  output.digits = new_mb
		  output.used = output_size
		  
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_shift_words_right(input as apmint_module.apmint, word_shift as int64, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = input shifted right 'word_shift' words (output is overwritten)
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  If input.Sign = 0 Then
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  var byte_shift As Int64 = word_shift * 8
		  
		  If byte_shift <= 0 Then
		    apmint_copy(input, output)
		    Return
		  End If
		  
		  If byte_shift >= input.used Then  ' if shift will shift off all bits, return 0
		    output.Sign = 0
		    output.used = 0
		    Return
		  End If
		  
		  output.Sign = input.Sign
		  output.used = input.used - byte_shift
		  
		  Var new_mb As New MemoryBlock(output.used)
		  new_mb.StringValue(0, output.used) = output.digits.StringValue(byte_shift, output.used)
		  output.digits = new_mb
		  
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_sqrt(input as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // square root of apmint; uses a newton iteration
		  
		  var x, last_x As apmint_module.apmint
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  // sqrt(0) = 0
		  If Input Is Nil Or Input.Sign = 0 Then
		    apmint_from_uint64(&h0, output)
		    Return
		  End If
		  
		  If Input.Sign = -1 Then  ' negative argument is invalid
		    var re As New RuntimeException
		    re.Message = "apmint_NegativeArgumentErr"
		    Raise re
		  End If
		  
		  var shift_amount As Int64 = apmint_get_bit_length(Input) \ 2 - 1
		  If shift_amount < 0 Then shift_amount = 0
		  apmint_shift_right(Input, shift_amount, last_x)
		  
		  While True
		    //x = (last_x + (Input \ last_x)) \ 2
		    apmint_divide(Input, last_x, x)
		    apmint_add(x, last_x, x)
		    apmint_divide_by2(x, x)
		    If x >= last_x Then
		      apmint_copy(last_x, output)
		      Return
		    End If
		    apmint_copy(x, last_x)
		  Wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_subtract(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // output = input1 - input2 (overwrites output)
		  
		  If output Is Nil Then output = New apmint_module.apmint
		  
		  // handle case of one or other of the inputs equal to zero
		  If input1.Sign * input2.Sign = 0 Then
		    If input1.Sign = 0 And input2.Sign = 0 Then
		      output.Sign = 0
		      output.used = 0
		      Return
		    Elseif input2.Sign = 0 Then
		      apmint_copy(input1, output)
		      Return
		    Elseif input1.Sign = 0 Then
		      apmint_copy(input2, output)
		      output.sign = -output.sign
		      Return
		    End If
		  End If
		  
		  
		  // if signs of input are different, add instead
		  If (input1.Sign * input2.Sign = -1) Then
		    var input1_sign as int64 = input1.sign ' input1 might be overwritten
		    apmint_add_abs(input1, input2, output)
		    output.Sign = input1_sign
		    Return
		  End If
		  
		  // signs of input the same
		  var input1_sign as int64 = input1.sign ' input1 might be overritten
		  apmint_subtract_abs(input1, input2, output)
		  output.sign = input1_sign * output.sign
		  return
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_subtract_abs(input1 as apmint_module.apmint, input2 as apmint_module.apmint, byref output as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // assumes both inputs are non-zero and output is not nil
		  // returns output = abs(input1) - abs(input2) and reuses (overwrites) output
		  // output can be same object as input1 or input2
		  
		  // handle special case of both values of Uint64 size
		  If input1.used + input2.used = 16 Then
		    var ui1 As UInt64 = input1.digits.UInt64Value(0)
		    var ui2 As UInt64 = input2.digits.UInt64Value(0)
		    If ui1 = ui2 Then
		      output.Sign = 0
		      output.used = 0
		      Return
		    Elseif ui1 > ui2 Then
		      output.Sign = 1
		      ui2 = ui1 - ui2
		    Else
		      output.Sign = -1
		      ui2 = ui2 - ui1
		    End If
		    If output.digits Is Nil Or output.digits.size < 8 Then output.digits = New memoryblock(8)
		    output.used = 8
		    output.digits.uint64Value(0) = ui2
		    Return
		  End If
		  
		  // find the largest and subtract smallest from largest
		  var a As apmint_module.apmint = input1
		  var b As apmint_module.apmint = input2
		  var swapped As Int64 = -1
		  
		  // compare 'a' and 'b'; if equal, output will be zero; else make 'a' smallest, 'b'largest
		  var icompare As Int64 = apmint_compare_abs(a, b)
		  If icompare = 0 Then ' inputs equal, return 0
		     output.Sign = 0
		     output.used = 0
		     Return
		  End If
		  If icompare = 1 Then                   ' if 'a' larger, swap
		    var c As apmint_module.apmint = a
		    a = b
		    b = c
		    swapped = 1
		  End If
		  
		  var asize As Int64 = a.used
		  var bsize As Int64 = b.used
		  If output.digits Is Nil Or output.digits.size < bsize Then apmint_grow(output, bsize)
		  a.used = asize ' set used values for a and b in case output is one of them (overwritting input)
		  b.used = bsize
		  var borrow, ui1, ui2 As UInt64
		  var dp As Int64
		  var aptr As ptr = a.digits
		  var bptr As ptr = b.digits
		  var rptr As ptr = output.digits
		  
		  // while both numbers have word in each column
		  While dp < asize
		    ui2 = bptr.UInt64(dp)
		    ui1 = ui2 - aptr.UInt64(dp) - borrow
		    If ui1 > ui2 Or ((ui1 = ui2) And (borrow = &h1)) Then
		      borrow = &h1
		    Else
		      borrow = &h0
		    End If
		    rptr.UInt64(dp) = ui1
		    dp = dp + 8
		  Wend
		  
		  // subtract those columns where there is b only
		  If bsize > asize Then
		    While dp < bsize
		      ui2 = bptr.UInt64(dp)
		      ui1 =  ui2 - borrow
		      If ui1  > ui2 Then
		        borrow = &h1
		      Else
		        borrow = &h0
		      End If
		      rptr.UInt64(dp) = ui1
		      dp = dp + 8
		    Wend
		  End If
		  
		  output.Sign = swapped
		  output.used = bsize
		  output.normalize
		  Return
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_swap(byref a as apmint_module.apmint, byref b as apmint_module.apmint)
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // swaps two apmints
		  
		  var c as apmint_module.apmint = a
		  a = b
		  b = c
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Sub apmint_test()
		  // apmint module tests
		  
		  var ii, jj, kk As Int64
		  var ui1 As UInt64
		  var s As String
		  var bigii, bigjj, bigkk, bigmm, bignn As apmint_module.apmint
		  
		  
		  // apmint creation
		  
		  system.DebugLog(" starting tests")
		  
		  bigii = 123
		  s = apmint_to_string(bigii)
		  If s <> "123" Then Break
		  
		  bigii = -123
		  s = apmint_to_string(bigii)
		  If s <> "-123" Then Break
		  
		  bigii = 123456789012345678
		  s = apmint_to_string(bigii)
		  If s <> "123456789012345678" Then Break
		  
		  bigii = "12345678901234567890"
		  s = apmint_to_string(bigii)
		  If s <> "12345678901234567890" Then Break
		  
		  bigii = "-12345678901234567890"
		  s = apmint_to_string(bigii)
		  If s <> "-12345678901234567890" Then Break
		  
		  // coversion to other bases
		  bigii = "1234567890123456789"
		  s = apmint_to_hex_string(bigii)
		  If s <> "112210F47DE98115" Then Break
		  s = apmint_to_octal_string(bigii)
		  If s <> "104420417217572300425" Then Break
		  s = apmint_to_binary_string(bigii)
		  If s <> "1000100100010000100001111010001111101111010011000000100010101" Then Break
		  
		  // comparisions
		  
		  bigii = 0
		  bigjj = 123
		  bigkk = 456
		  If bigii > bigjj Then Break
		  If bigkk < bigjj Then Break
		  If bigjj = bigkk Then Break
		  If bigjj <> bigjj Then Break
		  If bigii <> bigii Then Break
		  
		  bigii = -789
		  bigjj = 123
		  bigkk = -456
		  If bigkk > bigjj Then Break
		  If bigkk <> bigkk Then Break
		  If bigii > bigkk Then Break
		  
		  bigii = 0
		  bigkk = 0
		  If bigii <> bigkk Then Break
		  
		  // abs
		  bigii = -123
		  apmint_abs(bigii, bigkk)
		  s = apmint_to_string(bigkk)
		  If s <> "123" Then Break
		  bigii = 123
		  apmint_abs(bigii, bigkk)
		  s = apmint_to_string(bigkk)
		  If s <> "123" Then Break
		  
		  // negate
		  bigii = 123
		  bigkk = -bigii
		  s = apmint_to_string(bigkk)
		  If s <> "-123" Then Break
		  
		  
		  // apmint add/subtract/multiply/divide/mod test
		  For loop1 As Int64 = 1 To 100000
		    
		    ii = Xojo.Math.RandomInt(-999999999,999999999)
		    jj = Xojo.Math.RandomInt(-999999999,999999999)
		    bigii = ii
		    bigjj = jj
		    kk = ii * jj
		    bigkk = bigii * bigjj
		    s = apmint_to_string(bigkk)
		    If bigkk <> kk Then
		      System.DebugLog "multiply function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		    ii = Xojo.Math.RandomInt(-999999999999999,999999999999999)
		    jj = Xojo.Math.RandomInt(-999999999999999,999999999999999)
		    bigii = ii
		    bigjj = jj
		    kk = ii + jj
		    bigkk = bigii + bigjj
		    If bigkk <> kk Then
		      System.DebugLog "add function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		    kk = ii - jj
		    bigkk = bigii - bigjj
		    If bigkk <> kk Then
		      System.DebugLog "subtract function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		    If jj = 0 Then Continue  // for this test avoid divide by zero
		    
		    kk = ii\jj
		    bigkk = bigii\bigjj
		    If bigkk <> kk Then
		      System.DebugLog "divide function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		    ii = Xojo.Math.RandomInt(0,999999)
		    jj = Xojo.Math.RandomInt(0,999999)
		    
		    If jj = 0 Then Continue  // for this test avoid divide by zero
		    
		    bigii = ii
		    bigjj = jj
		    kk = ii Mod jj
		    bigkk = bigii Mod bigjj
		    If bigkk <> kk Then
		      System.DebugLog "mod function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		  Next
		  
		  // FFT multiply
		  
		  bigii = "12345678123456781234567891234567812345678123456789"
		  bigjj =  "23456789234567892345678902345678923456789234567890"
		  bigkk = apmint_fft_multiply(bigii, bigjj)
		  s = apmint_to_string(bigkk)
		  If s <> "289589969699741365035831920591555762842223197687375430612061576203688462518626735618228939501905210" Then Break
		  
		  
		  // bitwise operators; and, or, xor
		  
		  For loop1 As Int64 = 1 To 10000
		    
		    ii = Xojo.Math.RandomInt(0,99999999999999999)
		    jj = Xojo.Math.RandomInt(0,99999999999999999)
		    bigii = ii
		    bigjj = jj
		    kk = ii And jj
		    bigkk = bigii And bigjj
		    If bigkk <> kk Then
		      System.DebugLog "and function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		    kk = ii Or jj
		    bigkk = bigii Or bigjj
		    If bigkk <> kk Then
		      System.DebugLog "and function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		    kk = ii Xor jj
		    bigkk = bigii Xor bigjj
		    If bigkk <> kk Then
		      System.DebugLog "and function failure"
		      System.DebugLog Str(ii)
		      System.DebugLog Str(jj)
		      System.DebugLog Str(kk)
		      System.DebugLog apmint_to_string(bigkk)
		      Break
		    End If
		    
		  Next
		  
		  // left and right shift
		  bigii = "1234567890"
		  
		  ii = 64
		  For ii = 0 To 500
		    apmint_shift_left(bigii, ii,  bigjj)
		    s = apmint_to_string(bigjj)
		    apmint_shift_right(bigjj, ii, bigjj)
		    s = apmint_to_string(bigjj)
		    If bigjj <> bigii Then Break
		  Next
		  
		  bigii = "123456789012345678901234567890"
		  
		  ii = 64
		  For ii = 0 To 500
		    apmint_shift_left(bigii, ii,  bigjj)
		    s = apmint_to_string(bigjj)
		    apmint_shift_right(bigjj, ii, bigjj)
		    s = apmint_to_string(bigjj)
		    If bigjj <> bigii Then Break
		  Next
		  
		  bigii = "1234567890123456789012345678901234567890"
		  
		  ii = 64
		  For ii = 0 To 500
		    apmint_shift_left(bigii, ii,  bigjj)
		    s = apmint_to_string(bigjj)
		    apmint_shift_right(bigjj, ii, bigjj)
		    s = apmint_to_string(bigjj)
		    If bigjj <> bigii Then Break
		  Next
		  
		  bigii = "7"
		  apmint_shift_right(bigii, 2, bigjj)
		  If bigjj <> "1" Then Break
		  
		  // set, clear, flip, get bit, bits
		  bigii = "7"
		  apmint_bit_set(bigii, 3, bigjj)
		  If bigjj <> 15 Then Break
		  apmint_bit_clear(bigjj, 1, bigkk)
		  If bigkk <> 13 Then Break
		  apmint_bit_set(bigkk, 48, bigii)
		  If bigii <> "281474976710669" Then Break
		  apmint_bit_clear(bigii, 48, bigkk)
		  If bigkk <> 13 Then Break
		  apmint_bit_flip(bigkk, 48, bigii)
		  If bigii <> "281474976710669" Then Break
		  apmint_bit_flip(bigii, 48, bigkk)
		  If bigkk <> 13 Then Break
		  bigii = "281474976710669"
		  ii = apmint_get_bit(bigii,48)
		  If ii <> 1 Then Break
		  ii = apmint_get_bit_length(bigii)
		  If ii <> 49 Then Break
		  
		  // to int64/uint64
		  bigii = "123456"
		  ii = apmint_to_int64(bigii)
		  If ii <> 123456 Then Break
		  ui1 = apmint_to_uint64(bigii)
		  If ui1 <> 123456 Then Break
		  bigii = 0
		  ii = apmint_to_int64(bigii)
		  If ii <> 0 Then Break
		  ui1 = apmint_to_uint64(bigii)
		  If ui1 <> 0 Then Break
		  bigii = -123456
		  ii = apmint_to_int64(bigii)
		  If ii <> -123456 Then Break
		  
		  // even, odd, one
		  bigii = "123456"
		  If apmint_is_odd(bigii) Then Break
		  bigii = "1234567"
		  If apmint_is_even(bigii) Then Break
		  bigii = "1"
		  If Not apmint_is_one(bigii) Then Break
		  
		  
		  //apmint add/subtract/divide/remainder tests
		  
		  For ii = 1 To 10000
		    apmint_random_inrange(-9999, 9999, bigii)
		    apmint_random_inrange(-9999, 9999, bigjj)
		    bigkk = bigii + bigjj
		    bigmm = bigkk - bigjj
		    If bigmm <> bigii Then
		      System.DebugLog "add/subtract failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      Break
		    End If
		    apmint_random_inrange("-999999999999999999999", "999999999999999999999", bigii)
		    apmint_random_inrange("-999999999999999999999", "999999999999999999999", bigjj)
		    bigkk = bigii + bigjj
		    bigmm = bigkk - bigjj
		    If bigmm <> bigii Then
		      System.DebugLog "add/subtract failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      Break
		    End If
		    apmint_random_inrange("-9999999999999999999999999999999999999999999999","9999999999999999999999999999999999999999999999",bigii)
		    apmint_random_inrange("-9999999999999999999999999999999999999999999999","9999999999999999999999999999999999999999999999",bigjj)
		    bigkk = bigii + bigjj
		    bigmm = bigkk - bigjj
		    If bigmm <> bigii Then
		      System.DebugLog "add/subtract failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      Break
		    End If
		    
		    apmint_random_inrange("-4294967295","4294967295",bigii)
		    Do
		      apmint_random_inrange("-4294967295","4294967295",bigjj)
		    Loop Until bigjj.Sign <> 0
		    apmint_divide_remainder(bigii, bigjj, bigkk, bigmm)
		    bignn = bigkk * bigjj + bigmm
		    If bignn <> bigii Then
		      System.DebugLog "divide/remainder failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      System.DebugLog apmint_to_string(bignn)
		      Break
		    End If
		    
		    apmint_random_inrange("-999999999999999999999","999999999999999999999",bigii)
		    Do
		      apmint_random_inrange("-4294967295","4294967295",bigjj)
		    Loop Until bigjj.Sign <> 0
		    apmint_divide_remainder(bigii, bigjj, bigkk, bigmm)
		    bignn = bigkk * bigjj + bigmm
		    If bignn <> bigii Then
		      System.DebugLog "divide/remainder failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      System.DebugLog apmint_to_string(bignn)
		      Break
		    End If
		    
		    apmint_random_inrange("-999999999999999999999","999999999999999999999",bigii)
		    Do
		      apmint_random_inrange("-999999999999999999999","999999999999999999999",bigjj)
		    Loop Until bigjj.Sign <> 0
		    apmint_divide_remainder(bigii, bigjj, bigkk, bigmm)
		    bignn = bigkk * bigjj + bigmm
		    If bignn <> bigii Then
		      System.DebugLog "divide/remainder failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      System.DebugLog apmint_to_string(bignn)
		      Break
		    End If
		    
		    apmint_random_inrange("-9999999999999999999999999999999999999999999999","9999999999999999999999999999999999999999999999",bigii)
		    Do
		      apmint_random_inrange("-9999999999999999999999999999999999999999999999","9999999999999999999999999999999999999999999999",bigjj)
		    Loop Until bigjj.Sign <> 0
		    apmint_divide_remainder(bigii, bigjj, bigkk, bigmm)
		    bignn = bigkk * bigjj + bigmm
		    If bignn <> bigii Then
		      System.DebugLog "divide/remainder failure"
		      System.DebugLog apmint_to_string(bigii)
		      System.DebugLog apmint_to_string(bigjj)
		      System.DebugLog apmint_to_string(bigkk)
		      System.DebugLog apmint_to_string(bigmm)
		      System.DebugLog apmint_to_string(bignn)
		      Break
		    End If
		  Next
		  
		  // test some add/subtract edge cases with over/underflow/borrow/carries
		  var big1 As New apmint_module.apmint
		  var big2 As New apmint_module.apmint
		  var big3 As New apmint_module.apmint
		  var big4 As New apmint_module.apmint
		  var big5 As New apmint_module.apmint
		  var big6 As New apmint_module.apmint
		  var big7 As New apmint_module.apmint
		  var big8 As New apmint_module.apmint
		  var big9 As New apmint_module.apmint
		  var big10 As New apmint_module.apmint
		  var big11 As New apmint_module.apmint
		  var big12 As New apmint_module.apmint
		  var big13 As New apmint_module.apmint
		  var big14 As New apmint_module.apmint
		  var big15 As New apmint_module.apmint
		  var big16 As New apmint_module.apmint
		  var big17 As New apmint_module.apmint
		  var big18 As New apmint_module.apmint
		  var big19 As New apmint_module.apmint
		  var big20 As New apmint_module.apmint
		                                                 ' word 1,          word2,            word3,          word4
		  apmint_bit_set_range(big1, 0, 64 * 3, big1) '          1,  all bits set,     all bits set,   all bits set
		  apmint_bit_clear_range(big1, 0, 63, big2)   '          1,  all bits set,     all bits set,              0
		  apmint_bit_clear_range(big1, 64, 127, big3) '          1,  all bits set,                0,   all bits set
		  apmint_bit_clear_range(big1, 128, 191, big4)'          1,             0,     all bits set,   all bits set
		  apmint_bit_clear_range(big1, 0, 127, big5)  '          1,  all bits set,                0,              0
		  apmint_bit_clear_range(big1, 64, 191, big6) '          1,             0,                0,    all bits set
		  apmint_bit_set(big2, 0 , big7)              '          1,  all bits set,     all bits set,               1
		  apmint_bit_set(big3, 64, big8)              '          1,  all bits set,                1,    all bits set
		  apmint_bit_set(big4, 128, big9)             '          1,             1,     all bits set,    all bits set
		  apmint_bit_set(big5, 0, big10)
		  apmint_bit_set(big10, 64, big10)            '          1,  all bits set,                1,               1
		  apmint_bit_set(big6, 64, big11)
		  apmint_bit_set(big11, 128, big11)           '          1,             1,                1,     all bits set
		  apmint_bit_clear(big10, 64, big12)          '          1,  all bits set,                0,                1
		  apmint_bit_clear(big11, 64, big13)          '          1,             1,                0,     all bits set
		  apmint_bit_clear(big11, 128, big14)         '          1,             0,                1,     all bits set
		  apmint_bit_clear(big11, 192, big15)         '                         1,                1,     all bits set
		  apmint_bit_clear(big15, 128, big16)         '                                           1,     all bits set
		  apmint_bit_clear(big16, 64, big17)          '                                                  all bits set
		  apmint_bit_clear(big1, 0, big18)
		  apmint_bit_clear(big18, 64, big18)          '                             all bits set -1,  all bits set -1
		  apmint_bit_clear(big10, 192, big19)         '               all bits set,               1,                1
		  apmint_bit_set_range(big20, 0, 127, big20)  '                                all bits set,     all bits set
		  
		  bigii = big1 + big2
		  If bigii <> "25108406941546723055343157692830665664390975033782428499967" Then Break
		  bigii = big1 + big3
		  If bigii <> "25108406941546723055002875325909727200964493914498079391742" Then Break
		  bigii = big1 + big4
		  If bigii <> "18831305206160042291847650636543937711770440940823871750142" Then Break
		  bigii = big1 + big5
		  If bigii <> "25108406941546723055002875325909727200946047170424369840127" Then Break
		  bigii = big1 + big6
		  If bigii <> "18831305206160042291507368269622999248325513077465813090302" Then Break
		  bigii = big1 + big7
		  If bigii <> "25108406941546723055343157692830665664390975033782428499968" Then Break
		  bigii = big1 + big8
		  If bigii <> "25108406941546723055002875325909727200982940658571788943358" Then Break
		  bigii = big1 + big9
		  If bigii <> "18831305206160042292187933003464876175233815548255639961598" Then Break
		  bigii = big1 + big10
		  If bigii <> "25108406941546723055002875325909727200964493914498079391744" Then Break
		  bigii = big1 + big11
		  If bigii <> "18831305206160042291847650636543937711807334428971290853374" Then Break
		  bigii = big1 + big12
		  If bigii <> "25108406941546723055002875325909727200946047170424369840128" Then Break
		  bigii = big1 + big13
		  If bigii <> "18831305206160042291847650636543937711788887684897581301758" Then Break
		  bigii = big1 + big14
		  If bigii <> "18831305206160042291507368269622999248343959821539522641918" Then Break
		  bigii = big1 + big15
		  If bigii <> "12554203470773361528011861213336271295704978984507256340478" Then Break
		  bigii = big1 + big16
		  If bigii <> "12554203470773361527671578846415332832241604377075488129022" Then Break
		  bigii = big1 + big17
		  If bigii <> "12554203470773361527671578846415332832223157633001778577406" Then Break
		  bigii = big1 + big18
		  If bigii <> "25108406941546723055343157692830665664390975033782428499965" Then Break
		  bigii = big19 + big18
		  If bigii <> "18831305206160042291167085902702060784843691725960335327231" Then Break
		  bigii = big19 + big20
		  If bigii <> "6277101735386680763835789423207666416120802188537744064512" Then Break
		  bigii = big1 - big2
		  If bigii <> "18446744073709551615" Then Break
		  bigii = big1 - big3
		  If bigii <> "340282366920938463444927863358058659840" Then Break
		  bigii = big1 - big4
		  If bigii <> "6277101735386680763495507056286727952638980837032266301440" Then Break
		  bigii = big1 - big5
		  If bigii <> "340282366920938463463374607431768211455" Then Break
		  bigii = big1 - big6
		  If bigii <> "6277101735386680763835789423207666416083908700390324961280" Then Break
		  bigii = big1 - big7
		  If bigii <> "18446744073709551614" Then Break
		  bigii = big1 - big8
		  If bigii <> "340282366920938463426481119284349108224" Then Break
		  bigii = big1 - big9
		  If bigii <> "6277101735386680763155224689365789489175606229600498089984" Then Break
		  bigii = big1 - big10
		  If bigii <> "340282366920938463444927863358058659838" Then Break
		  bigii = big1 - big11
		  If bigii <> "6277101735386680763495507056286727952602087348884847198208" Then Break
		  bigii = big1 - big12
		  If bigii <> "340282366920938463463374607431768211454" Then Break
		  bigii = big1 - big13
		  If bigii <> "6277101735386680763495507056286727952620534092958556749824" Then Break
		  bigii = big1 - big14
		  If bigii <> "6277101735386680763835789423207666416065461956316615409664" Then Break
		  bigii = big1 - big15
		  If bigii <> "12554203470773361527331296479494394368704442793348881711104" Then Break
		  bigii = big1 - big16
		  If bigii <> "12554203470773361527671578846415332832167817400780649922560" Then Break
		  bigii = big1 - big17
		  If bigii <> "12554203470773361527671578846415332832186264144854359474176" Then Break
		  bigii = big1 - big18
		  If bigii <> "18446744073709551617" Then Break
		  bigii = big19 - big20
		  If bigii <> "6277101735386680763155224689365789489194052973674207641602" Then Break
		  
		  
		  // prime number functions
		  If Not apmint_is_likely_prime(1000003) Then Break
		  If apmint_is_likely_prime(1000031) Then Break
		  bigii = "12"
		  apmint_next_prime(bigii, bigjj)
		  If bigjj <> "13" Then Break
		  apmint_previous_prime(bigii, bigjj)
		  If bigjj <> "11" Then Break
		  
		  // power methods
		  bigii = 0
		  bigjj = "4"
		  apmint_pow(bigii, bigjj, bigkk)
		  If bigkk.Sign <> 0 Then Break
		  bigii = 1
		  apmint_pow(bigii, bigjj, bigkk)
		  If Not apmint_is_one(bigkk) Then Break
		  bigii = -1
		  apmint_pow(bigii, bigjj, bigkk)
		  If Not apmint_is_one(bigkk) Then Break
		  bigii = "7"
		  bigjj = "4"
		  apmint_pow(bigii, bigjj, bigkk)
		  If bigkk <> "2401" Then Break
		  bigjj = "-4"
		  apmint_pow(bigii, bigjj, bigkk)
		  If bigkk.Sign <> 0 Then Break
		  var base As apmint_module.apmint
		  var power As apmint_module.apmint
		  bigii = "121"
		  If Not apmint_is_power(bigii, base, power) Then Break
		  If base <> "11" Then Break
		  If power <> "2" Then Break
		  apmint_sqrt(bigii, bigjj)
		  If bigjj <> "11" Then Break
		  If Not apmint_is_square(bigii, bigjj) Then Break
		  If bigjj <> "11" Then Break
		  bigii = "122"
		  If apmint_is_square(bigii, bigjj) Then Break
		  bigii = "28561"
		  apmint_nroot(bigii, 4, bigjj)
		  If bigjj <> "13" Then Break
		  
		  //get methods
		  bigii = "123"
		  ii = apmint_get_least_significant_digit(bigii)
		  If ii <> 3 Then Break
		  ii = apmint_get_bit_length(bigii)
		  If ii <> 7 Then Break
		  ii = apmint_get_bit(bigii, 6)
		  If ii <> 1 Then Break
		  ii = apmint_get_bit(bigii, 2)
		  If ii <> 0 Then Break
		  ii = apmint_get_bits_set(bigii)
		  If ii <> 6 Then Break
		  ii = apmint_get_bits_clear(bigii)
		  If ii <> 1 Then Break
		  
		  // lcm
		  bigii = "347236"
		  bigjj = "297228"
		  apmint_lcm(bigii, bigjj, bigkk)     // bigkk = 548980116
		  If bigkk <> "548980116" Then Break
		  
		  
		  MsgBox "tests passed"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_to_binary_string(input as apmint_module.apmint) As String
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // convert an apmint to a binary string; will convert to hex, then binary
		  
		  If input is nil or input.Sign = 0 Then Return "0"
		  
		  var hex_string As String = apmint_to_hex_string(input)
		  var string_length As Int64 = hex_string.Length
		  var mb_out As New memoryblock(string_length * 4)
		  var mb_in As memoryblock = hex_string
		  var out_pos, loopvar As Int64
		  string_length = string_length - 1
		  var temp_byte As Int8
		  For loopvar = 0 To string_length
		    temp_byte = mb_in.int8value(loopvar)
		    Select Case mb_in.Int8Value(loopvar)
		    Case 48
		      mb_out.UInt32Value(out_pos) = &h30303030
		    Case 49
		      mb_out.UInt32Value(out_pos) = &h31303030
		    Case 50
		      mb_out.UInt32Value(out_pos) = &h30313030
		    Case 51
		      mb_out.UInt32Value(out_pos) = &h31313030
		    Case 52
		      mb_out.UInt32Value(out_pos) = &h30303130
		    Case 53
		      mb_out.UInt32Value(out_pos) = &h31303130
		    Case 54
		      mb_out.UInt32Value(out_pos) = &h30313130
		    Case 55
		      mb_out.UInt32Value(out_pos) = &h31313130
		    Case 56
		      mb_out.UInt32Value(out_pos) = &h30303031
		    Case 57
		      mb_out.UInt32Value(out_pos) = &h31303031
		    Case 65
		      mb_out.UInt32Value(out_pos) = &h30313031
		    Case 66
		      mb_out.UInt32Value(out_pos) = &h31313031
		    Case 67
		      mb_out.UInt32Value(out_pos) = &h30303131
		    Case 68
		      mb_out.UInt32Value(out_pos) = &h31303131
		    Case 69
		      mb_out.UInt32Value(out_pos) = &h30313131
		    Case 70
		      mb_out.UInt32Value(out_pos) =  &h31313131
		    End Select
		    out_pos = out_pos + 4
		  Next
		  
		  // count leading zeros
		  var leading_zeros As Int64
		  For loopvar = 0 To 3
		    If mb_out.Uint8Value(loopvar) <> &h30 Then Exit
		    leading_zeros = leading_zeros + 1
		  Next
		  
		  return mb_out.StringValue(leading_zeros, string_length*4 + 1)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_to_hex_string(input as apmint_module.apmint) As String
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // convert an apmint to hex as string
		  
		  If input is nil or input.Sign = 0 Then Return "0"
		  
		  var total_hex_digits as int64 = input.used * 2 ' two hex digits per byte
		  var mb As New memoryblock(total_hex_digits)
		  var mb_ptr as ptr = mb
		  var ptr1 as Int64
		  var input_ptr as ptr = input.digits
		  var ptr2 as Int64 = input.used - 8 ' start with most significant word
		  var ui1, hex_digit as UInt64
		  var digits(15) as UInt64
		  
		  while ptr2 >= 0
		    ui1 = input_ptr.UInt64(ptr2)
		    digits(15) =              ui1 and &hf
		    digits(14) = (ui1 \ &h10) and &hf
		    digits(13) = (ui1 \ &h100) and &hf
		    digits(12) = (ui1 \ &h1000) and &hf
		    digits(11) = (ui1 \ &h10000) and &hf
		    digits(10) = (ui1 \ &h100000) and &hf
		    digits(9) =   (ui1 \ &h1000000) and &hf
		    digits(8) =   (ui1 \ &h10000000) and &hf
		    digits(7) =   (ui1 \ &h100000000) and &hf
		    digits(6) =   (ui1 \ &h1000000000) and &hf
		    digits(5) =   (ui1 \ &h10000000000) and &hf
		    digits(4) =   (ui1 \ &h100000000000) and &hf
		    digits(3) =   (ui1 \ &h1000000000000) and &hf
		    digits(2) =   (ui1 \ &h10000000000000) and &hf
		    digits(1) =   (ui1 \ &h100000000000000) and &hf
		    digits(0) =   (ui1 \ &h1000000000000000) and &hf
		    
		    for ii as int64 = 0 to 15
		      hex_digit = digits(ii)
		      select case hex_digit
		      Case 0 To  9
		        mb_ptr.Int8(ptr1) = hex_digit + 48
		      Case 10 To 15
		        mb_ptr.Int8(ptr1) = (hex_digit - 10) + Asc("A")
		      End Select
		      ptr1 = ptr1 + 1
		    next
		    
		    ptr2 = ptr2 - 8
		    
		  wend
		  
		  // convert to text, skip leading zeros
		  ptr1 = 0
		  while mb_ptr.int8(ptr1) = 48
		    ptr1 = ptr1 + 1
		  wend
		  
		  Return mb.StringValue(mb.size - total_hex_digits - ptr1, mb.size)  // right total_hex_digits - ptr1 bytes
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_to_int64(input as apmint_module.apmint) As int64
		  #pragma DisableBoundsChecking
		  #pragma StackOverflowChecking False
		  #pragma Disablebackgroundtasks
		  #pragma NilObjectChecking false
		  
		  // convert a apmint to an int64
		  // valid only for values within range of int64
		  // will throw exception if outside the in64 range
		  
		  var result As Int64
		  Static max_positive as apmint_module.apmint   = "9223372036854775807"
		  Static max_negative as apmint_module.apmint   = "-9223372036854775808"
		  
		  If input is nil or input.Sign = 0 Then
		    Return 0
		  Elseif input > max_positive or input < max_negative Then
		    var re As New RuntimeException
		    re.Message = "apmint_UnderOverflowErr"
		    Raise re
		  Else
		    result = input.digits.Int64Value(0)
		    If input.Sign = -1 Then
		      result = -result
		    End If
		    Return result
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_to_octal_string(input as apmint_module.apmint) As String
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking false
		  
		  // convert to octal string
		  
		  If input is nil or input.Sign = 0 Then Return "0"
		  
		  var total_octal_digits as Int64 = 22 * (input.used \ 8)  ' possible octal digits (padded)
		  var mb As New memoryblock(total_octal_digits)
		  var mb2 As New memoryblock(total_octal_digits)
		  var mb_ptr As ptr = mb
		  var mb2_ptr As ptr = mb2
		  var octal_digit_count As Int64
		  var ui1 as UInt64
		  
		  var b1 as apmint_module.apmint
		  apmint_copy(input, b1)
		  
		  While b1.Sign <> 0
		    ui1 = b1.digits.UInt64Value(0) And &h7
		    mb_ptr.Int8(octal_digit_count) = ui1 + 48
		    octal_digit_count = octal_digit_count + 1
		    apmint_shift_right(b1, 3, b1)
		  Wend
		  
		  // reverse octal digits
		  octal_digit_count = octal_digit_count - 1
		  var ptr1 As Int64 = octal_digit_count
		  For ii As Int64 = 0 To octal_digit_count
		    mb2_ptr.Int8(ii) = mb_ptr.Int8(ptr1)
		    ptr1 = ptr1 - 1
		  Next
		  
		  // convert to string
		  Return mb2.StringValue(0, octal_digit_count + 1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_to_string(input as apmint_module.apmint) As String
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // convert to base-10 string
		  
		  If input is nil or  input.Sign = 0 Then Return "0"
		  
		  // strategy:
		  // convert input to base 100000000 (0 - 99999999)
		  // convert to base-10 digits
		  // convert to text
		  
		  var input_ptr As ptr = input.digits
		  var digits As Double = apmint_get_bit_length(input) / 3.32 + 15      ' estimate number of base-10 digits (padded)
		  var base10_digits As Int64 = digits
		  var base10kk_digits As Int64 = (base10_digits + 7) \ 8               ' estimate number of words in base 100000000 number
		  var base10kk_mb As New memoryblock(base10kk_digits * 8)
		  var base10kk_ptr As ptr = base10kk_mb
		  var base10_mb As New memoryblock(base10_digits)
		  var base10_ptr As ptr = base10_mb
		  var base10kk As UInt64 = CType(100000000, UInt64)
		  var p1, p2, ii, jj As Int64
		  
		  // convert input to base10kk
		  
		  var carry, uitemp As UInt64
		  p1 = input.used - 2
		  While True
		    // add next apmint_word
		    base10kk_ptr.UInt64(0) = base10kk_ptr.UInt64(0) + CType(input_ptr.UInt16(p1), UInt64)
		    If p1 = 0 Then Exit
		    // multiply by apmint_base
		    carry = 0
		    For ii = 0 To p2 Step 8
		      uitemp = base10kk_ptr.UInt64(ii)
		      uitemp = uitemp * &h10000 + carry
		      carry = uitemp \ base10kk
		      base10kk_ptr.UInt64(ii) = uitemp Mod base10kk
		    Next
		    While carry <> 0
		      p2 = p2 + 8
		      base10kk_ptr.UInt64(p2) = carry
		      carry = base10kk_ptr.UInt64(p2) \ base10kk
		      If carry = 0 Then Exit
		      base10kk_ptr.UInt64(p2) = base10kk_ptr.UInt64(p2) Mod base10kk
		    Wend
		    p1 = p1 - 2
		  Wend
		  
		  // remove any remaining carries
		  
		  For ii = 0 To p2 Step 8
		    carry = base10kk_ptr.UInt64(ii) \ base10kk
		    If carry = 0 Then Exit
		    base10kk_ptr.UInt64(ii) = base10kk_ptr.UInt64(ii) Mod base10kk
		    base10kk_ptr.UInt64(ii + 8) = base10kk_ptr.UInt64(ii + 8) + carry
		  Next
		  If ii > p2 And carry <> 0 Then p2 = p2 + 8
		  
		  // convert to base-10 (as ascii)
		  
		  p1 = base10_digits - 1
		  For jj = 0 To p2 Step 8
		    uitemp = base10kk_ptr.UInt64(jj)
		    For ii = 1 To 8
		      base10_ptr.Int8(p1) = (uitemp Mod 10) + 48
		      uitemp = uitemp \ 10
		      p1 = p1 - 1
		    Next
		  Next
		  
		  // find first non-zero digit
		  
		  While True
		    p1 = p1 + 1
		    If base10_ptr.Int8(p1) <> 48 Then Exit
		  Wend
		  
		  // prepend the sign (if negative)
		  
		  If input.Sign = -1 Then
		    p1 = p1 - 1
		    base10_ptr.Byte(p1) = 45
		  End If
		  
		  // return final string
		  Return base10_mb.StringValue(p1, base10_digits - p1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0, CompatibilityFlags = API2Only
		Function apmint_to_uint64(input as apmint_module.apmint) As uint64
		  #pragma DisableBoundsChecking
		  #pragma Disablebackgroundtasks
		  #pragma StackOverflowChecking False
		  #pragma NilObjectChecking False
		  
		  // convert to Uint64
		  
		  If Input Is Nil Or Input.Sign = 0 Then Return &h0
		  
		  If Input.used > 8 Then
		    var re As New RuntimeException
		    re.Message = "apmint_UnderOverflowErr"
		    Raise re
		  End If
		  
		  Return Input.digits.UInt64Value(0)
		End Function
	#tag EndMethod


	#tag Note, Name = Constants
		If constants apmint_base or apmint_base_bits are changed then the apmint_fft_multiply method will need to be
		rewritten since it depends on 30 bit words that are broken down into 10 bit chunks.
		
	#tag EndNote


	#tag Constant, Name = limit_bytes, Type = Boolean, Dynamic = False, Default = \"true", Scope = Public
	#tag EndConstant

	#tag Constant, Name = max_bytes, Type = Double, Dynamic = False, Default = \"442928", Scope = Public
	#tag EndConstant


	#tag Using, Name = Xojo.Math
	#tag EndUsing


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
	#tag EndViewBehavior
End Module
#tag EndModule
