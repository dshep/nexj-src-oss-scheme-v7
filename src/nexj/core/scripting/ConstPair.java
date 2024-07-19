// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.ObjectStreamException;

/**
 * Read-only pair implementation.
 */
public class ConstPair extends Pair
{
   // attributes
   
   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 4905439272318294107L;

   // constructors

   /**
    * Constructs the pair.
    * @param head The pair head.
    */
   public ConstPair(Object head)
   {
      super(head);
   }

   /**
    * Constructs the pair.
    * @param head The pair head.
    * @param tail The pair tail.
    */
   public ConstPair(Object head, Object tail)
   {
      super(head, tail);
   }
   
   // operations

   /**
    * Prevents modification.
    * @see nexj.core.scripting.Pair#setHead(java.lang.Object)
    */
   public void setHead(Object head)
   {
      if (head != m_head)
      {
         throw new ScriptingException("err.scripting.readOnlyPair");
      }
   }

   /**
    * Prevents modification.
    * @see nexj.core.scripting.Pair#setTail(java.lang.Object)
    */
   public void setTail(Object tail)
   {
      if (tail != m_tail)
      {
         throw new ScriptingException("err.scripting.readOnlyPair");
      }
   }
   
   /**
    * Replaces the object with Pair during serialization.
    */
   private Object writeReplace() throws ObjectStreamException
   {
      return new Pair(m_head, m_tail);
   }
}
