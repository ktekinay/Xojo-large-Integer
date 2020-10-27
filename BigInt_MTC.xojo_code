#tag Class
Protected Class BigInt_MTC
	#tag Method, Flags = &h0
		Sub Add(value As BigInt_MTC)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Add(value As Int64)
		  AddInt64 value, 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AddInt64(value As Int64, block As Integer)
		  if value = 0 then
		    return
		    
		  elseif value < 0 and Sign >= 0 then
		    SubtractInt64 value, block
		    
		  elseif value >= 0 and Sign < 0 then
		    SubtractInt64 -value, 0
		    
		  else
		    
		    const kMaxUInt32 as UInt64 = &hFFFFFFFF
		    
		    var valueSign as integer = if( value < 0, -1, 1 )
		    if valueSign = -1 then
		      value = 0 - value
		    end if
		    
		    var p as Ptr = Data
		    var firstByte as integer = block * 8
		    var lastByte as integer = Data.Size - 1
		    
		    var startByte as integer = 8 * block
		    
		    while value > 0
		      var word as UInt32 = value and kMaxUInt32
		      value = value \ CType( 256^4, Int64 )
		      
		      if startByte >= Data.Size then
		        Data.Size = Data.Size + 16
		      end if
		      
		      var dataWord as UInt64 = p.UInt32( startByte )
		      dataWord = dataWord + word
		      if dataWord > kMaxUInt32 then
		        var carry as UInt64 = ( dataWord and &hFFFFFFFF00000000 ) \ CType( 256^4, UInt64 )
		        value = value + carry
		        dataWord = dataWord and kMaxUInt32
		      end if
		      p.UInt32( startByte ) = dataWord
		      startByte = startByte + 4
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
		  
		  Data = new MemoryBlock( 16 )
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(value As Int64)
		  Constructor
		  
		  AddInt64 value, 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(value As String)
		  Constructor
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub SubtractInt64(value As Int64, block As Integer)
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected Data As MemoryBlock
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected Sign As Integer = 0
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  if Sign = 0 then
			    return "0"
			  end if
			  
			  var p as Ptr = Data
			  
			  var parts() as string
			  
			  if Sign < 0 then
			    parts.AddRow "-"
			  end if
			  
			  var foundValue as boolean
			  
			  var lastByte as integer = Data.Size - 4
			  for block as integer = lastByte downto 0 step 4
			    var thisValue as UInt64 = p.UInt32( block )
			    if foundValue or thisValue > 0 then
			      parts.AddRow thisValue.ToString
			      foundValue = true
			    end if
			  next
			  
			  var result as string = String.FromArray( parts, "" )
			  return result
			  
			End Get
		#tag EndGetter
		ToString As String
	#tag EndComputedProperty


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
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
