#tag Class
Protected Class BigInt_MTC
	#tag Method, Flags = &h0
		Sub Add(value As BigInt_MTC)
		  var copyFromValues() as UInt64 = value.Values
		  
		  var startingIndex as integer
		  for each item as UInt64 in copyFromValues
		    AddInt64 item, startingIndex
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Add(value As Int64)
		  var startingIndex as integer
		  AddInt64 value, startingIndex
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AddInt64(value As Int64, ByRef startingIndex As Integer)
		  //
		  // Subtract will go through here too
		  //
		  
		  #if not DebugBuild then
		    #pragma BackgroundTasks false
		    #pragma BoundsChecking false
		    #pragma NilObjectChecking false
		    #pragma StackOverflowChecking false
		  #endif
		  
		  if value = 0 then
		    return
		  end if
		  
		  var valueSign as integer = 1
		  if value < 0 then
		    valueSign = -1
		    value = -value
		  end if
		  
		  if Sign = 0 and valueSign = -1 then
		    SubtractInt64 value, startingIndex
		    
		  elseif Sign <> 0 and valueSign <> Sign then
		    SubtractInt64 value, startingIndex
		    
		  else
		    
		    while value > 0
		      while startingIndex > Values.LastRowIndex
		        Values.ResizeTo Values.LastRowIndex + kAddArrayCount
		      wend
		      
		      var valueCarry as Int64 = value \ ( kMaxElementValue + 1 )
		      var word as UInt64 = value - ( valueCarry * ( kMaxElementValue + 1 ) )
		      value = valueCarry
		      
		      var elementValue as Int64 = Values( startingIndex ) + word
		      
		      if elementValue > kMaxElementValue then
		        var carry as Int64 = elementValue / ( kMaxElementValue + 1 )
		        elementValue = elementValue - ( carry * ( kMaxElementValue + 1 ) )
		        value = value + carry
		      end if
		      
		      Values( startingIndex ) = elementValue
		      
		      startingIndex = startingIndex + 1
		    wend
		    
		    if Sign = 0 then
		      Sign = valueSign
		    end if
		    
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  //
		  // Basic constructor
		  //
		  
		  Values.ResizeTo kAddArrayCount - 1
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(copyFrom As BigInt_MTC)
		  Constructor
		  
		  var copyFromValues() as UInt64 = copyFrom.Values
		  Values.ResizeTo copyFromValues.LastRowIndex
		  Sign = copyFrom.Sign
		  
		  for i as integer = 0 to Values.LastRowIndex
		    Values( i ) = copyFromValues( i )
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(value As Int64)
		  Constructor
		  
		  var startingIndex as integer
		  AddInt64 value, startingIndex
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(value As String)
		  Constructor
		  
		  var valueSign as integer
		  var arr() as UInt64 = StringToArray( value , valueSign )
		  
		  //
		  // Make sure it's a multiple
		  //
		  var diff as integer = arr.Count mod kAddArrayCount
		  if diff <> 0 then
		    arr.ResizeTo arr.LastRowIndex + diff
		  end if
		  
		  Values = arr
		  Sign = valueSign
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Attributes( hidden )  Sub Operator_Convert(value As Int64)
		  Constructor value
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Attributes( hidden )  Sub Operator_Convert(value As String)
		  Constructor value
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function StringToArray(value As String, ByRef returnSign As Integer) As UInt64()
		  //
		  // The first element will contain the sign
		  //
		  
		  var arr() as UInt64
		  
		  if value = "" then
		    returnSign = 0
		    arr.ResizeTo 0
		    return arr
		  end if
		  
		  static rxValidate as RegEx
		  if rxValidate is nil then
		    rxValidate = new RegEx
		    rxValidate.SearchPattern = "\A([-+])?\d+\z"
		  end if
		  
		  var match as RegExMatch = rxValidate.Search( value )
		  
		  if match is nil then
		    raise new InvalidArgumentException
		  end if
		  
		  var startIndex as integer = 0
		  
		  if match.SubExpressionCount = 1 then
		    returnSign = 1
		    
		  elseif match.SubExpressionCount = 2 and match.SubExpressionString( 1 ) = "-" then
		    returnSign = -1
		    startIndex = 1
		    
		  elseif match.SubExpressionString( 1 ) = "+" then
		    returnSign = 1
		    startIndex = 1
		  end if
		  
		  var length as integer = value.Bytes - startIndex
		  var lastStartIndex as integer = -1
		  
		  while length > kMaxElementDigits
		    lastStartIndex = length - kMaxElementDigits
		    var chunk as string = value.MiddleBytes( lastStartIndex, kMaxElementDigits )
		    arr.AddRow chunk.ToInt64
		    length = length - kMaxElementDigits
		  wend
		  
		  if lastStartIndex = -1 then
		    //
		    // Didn't process it
		    //
		    arr.AddRow value.ToInt64
		    
		  elseif lastStartIndex > startIndex then
		    length = lastStartIndex - startIndex
		    var chunk as string = value.MiddleBytes( startIndex, length )
		    arr.AddRow chunk.ToInt64
		  end if
		  
		  return arr
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Subtract(value As Int64)
		  var startingIndex as integer
		  AddInt64 -value, startingIndex
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub SubtractInt64(value As Int64, startingIndex As Integer)
		  //
		  // We only get here from AddInt64 so
		  // we are assured of a positive integer in value
		  //
		  
		  #if not DebugBuild then
		    #pragma BackgroundTasks false
		    #pragma BoundsChecking false
		    #pragma NilObjectChecking false
		    #pragma StackOverflowChecking false
		  #endif
		  
		End Sub
	#tag EndMethod


	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  const kZero as UInt64 = 0
			  
			  for index as integer = Values.LastRowIndex downto 0
			    if Values( index ) <> kZero then
			      return index
			    end if
			  next
			  
			  //
			  // No element had value
			  //
			  Sign = 0 // So make sure this is right
			  return -1
			  
			End Get
		#tag EndGetter
		Private LastIndexWithValue As Integer
	#tag EndComputedProperty

	#tag Property, Flags = &h1
		Protected Sign As Integer = 0
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  static formatString as string
			  if formatString = "" then
			    formatString = "0"
			    
			    while formatString.Bytes < kMaxElementDigits
			      formatString = formatString + formatString
			    wend
			    
			    formatString = formatString.LeftBytes( kMaxElementDigits )
			  end if
			  
			  var lastIndex as integer = LastIndexWithValue
			  if lastIndex = -1 then
			    return "0"
			  end if
			  
			  var parts() as string
			  
			  parts.AddRow Values( lastIndex ).ToString
			  lastIndex = lastIndex - 1
			  
			  for index as integer = lastIndex downto 0
			    parts.AddRow Values( index ).ToString( formatString )
			  next
			  
			  var result as string = if( Sign < 0, "-", "" ) + String.FromArray( parts, "" )
			  return result
			  
			End Get
		#tag EndGetter
		ToString As String
	#tag EndComputedProperty

	#tag Property, Flags = &h1
		Protected Values() As UInt64
	#tag EndProperty


	#tag Constant, Name = kAddArrayCount, Type = Double, Dynamic = False, Default = \"2", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kMaxElementDigits, Type = Double, Dynamic = False, Default = \"12", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kMaxElementValue, Type = Double, Dynamic = False, Default = \"999999999999", Scope = Private
	#tag EndConstant


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
			Name="ToString"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
