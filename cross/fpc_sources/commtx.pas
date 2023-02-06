{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 

  %W% %G% %U%

  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Tree Processing Common Routine Declarations

 Last modified by KRIS on 21-Nov-1990 15:23:00
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

procedure treadaccess(i: nodeindex; {node wanted}
                      var p: nodeptr {provides access to desired node} );

{ Make virtual element "i" available for read-only access and return a
  pointer to it.
}
  external;


procedure twriteaccess(i: nodeindex; {node to access}
                       var p: nodeptr {provides access to the node} );

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.
}
  external;


procedure flushbuffers;

{ Return all buffers used in the virtual memory system to the
  heap.
}
  external;


procedure tdecreasebuffers;

{ If available memory is running low relenquish a cache buffer.
}
  external;


procedure tincreasebuffers;

{ Add a new virtual buffer if space has gotten large due to the
  dispose routine.
}
  external;


function newlabel: labelrange;

{ Create a new pseudo-code label.
}
  external;


procedure checkconst(node: nodeindex; {node to check}
                     var constflag: boolean; {true if node is const}
                     var i: integer {constant value if const} );

{ Check node "node" to see if it is constant, and set "constflag" and
  "i" if it is.  This is used in dead code elimination.
}
  external;


procedure estimateloop(stmt: nodeindex;
                       var fixed: boolean;
                       var overflow: boolean;
                       var runcount: unsignedint);

{ Figure out if possible how many iterations a for loop will execute.
}
  external;


procedure putpseudofile;

{ Do the equivalent of a "put" on the pseudofile.
}
  external;


procedure genpseudo(o: pseudoop; {operator}
                    l: addressrange; {operand length}
                    n: keyindex; {key for this node}
                    r: refcountrange; {reference count}
                    c: refcountrange; {copy count}
                    i, j, k: integer {operands} );

{ Generate a pseudo instruction
}
  external;


procedure genrealop(o: pseudoop; {operator}
                    l: addressrange; {operand length}
                    n: keyindex; {key for this node}
                    r: refcountrange; {reference count}
                    c: refcountrange; {copy count}
                    val: realarray {real value} );

{ Generate a pseudo instruction with a real value as an operand
}
  external;

  procedure increfcount(n: nodeindex; {node to increment}
                        deadcode: boolean;
                        inc: shortint {amount to increment} );

{ Change the effective reference count for a node by "inc".  This
  may have to chain down a sequence of nodes to get the actual most
  recent node which should be changed.
}

  external;
