// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.scripting.Symbol;
import nexj.core.util.UncheckedException;

/**
 * Exception thrown when an operator type mismatch occurs.
 */
public class TypeMismatchException extends UncheckedException
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 4363973319290106800L;

   // constructors

   /**
    * Constructs the exception.
    * @param sym The operator symbol.
    */
   public TypeMismatchException(Symbol sym)
   {
      this(sym.getName());
   }

   /**
    * Constructs the exception.
    * @param sName The operator name.
    */
   public TypeMismatchException(String sName)
   {
      super("err.meta.typeMismatch", new Object[]{sName});
   }
}
