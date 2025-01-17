// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Metaclass object for exceptions.
 */
public class ExceptionMetaclassObject extends BasicMetaclassObject
{
   // constants

   /**
    * The exception metaclass symbol.
    */
   public final static Symbol METACLASS_SYMBOL = Symbol.define("sys:ExceptionMetaclass");

   // constructors

   /**
    * @see BasicMetaclassObject#BasicMetaclassObject(Symbol)
    */
   public ExceptionMetaclassObject(Symbol symbol)
   {
      super(symbol);
   }

   /**
    * Constructs the root exception metaclass. 
    */
   public ExceptionMetaclassObject()
   {
      super(METACLASS_SYMBOL);
      ObjectException.addClassMembers(this);
   }

   // operations

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createInstance(ClassObject)
    */
   protected ObjectOriented createInstance(ClassObject classObject)
   {
      return new ObjectException(classObject);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createMetaclass(nexj.core.scripting.Symbol)
    */
   protected MetaclassObject createMetaclass(Symbol symbol)
   {
      return new ExceptionMetaclassObject(symbol);
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#addObjectMembers(nexj.core.scripting.object.ClassObject)
    */
   protected void addObjectMembers(ClassObject classObject)
   {
      classObject.addMethod("code", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ObjectException)machine.getArg(0, nArgCount)).getErrorCode(), nArgCount);

            return false;
         }
      });

      classObject.addMethod("args", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ObjectException)machine.getArg(0, nArgCount)).getErrorArgs(), nArgCount);

            return false;
         }
      });

      classObject.addMethod("cause", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ObjectException)machine.getArg(0, nArgCount)).getCause(), nArgCount);

            return false;
         }
      });

      classObject.addMethod("throw", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            throw (ObjectException)machine.getArg(0, nArgCount);
         }
      });
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#getObjectClassSymbol()
    */
   protected Symbol getObjectClassSymbol()
   {
      return ObjectException.CLASS_SYMBOL;
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#getObjectBaseClassSymbol()
    */
   protected Symbol getObjectBaseClassSymbol()
   {
      return BasicObject.CLASS_SYMBOL;
   }

   /**
    * Initializes the exception system. 
    */
   public static void init()
   {
      new ExceptionMetaclassObject().define();
   }
}
