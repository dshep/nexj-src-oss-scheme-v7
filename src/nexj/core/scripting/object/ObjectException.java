// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.meta.Primitive;
import nexj.core.meta.TypeMismatchException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.ScriptingError;
import nexj.core.scripting.Symbol;

/**
 * Base class for deriving object system exceptions.
 */
public class ObjectException extends ScriptingError implements ObjectOriented
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 1999986045791940176L;

   /**
    * The exception class symbol.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:Exception");

   /**
    * Empty object array.
    */
   protected final static Object[] EMPTY_ARRAY = new Object[0];

   // attributes

   /**
    * The object attributes indexed by attribute offset: Object[].
    */
   protected Object[] m_valueArray;

   // associations

   /**
    * The class object.
    */
   protected ClassObject m_class;

   // constructors

   public ObjectException(String sErrCode)
   {
      this();
      init(sErrCode, null, null);
   }

   public ObjectException(String sErrCode, Object[] argArray)
   {
      this();
      init(sErrCode, argArray, null);
   }

   public ObjectException(String sErrCode, Throwable cause)
   {
      this();
      init(sErrCode, null, cause);
   }

   public ObjectException(String sErrCode, Object[] argArray, Throwable cause)
   {
      this();
      init(sErrCode, argArray, cause);
   }

   public ObjectException(ClassObject classObject, String sErrCode, Object[] argArray, Throwable cause)
   {
      this(classObject);
      init(sErrCode, argArray, cause);
   }

   public ObjectException(ClassObject classObject)
   {
      super("err.scripting.object");
      m_class = classObject;

      int nAttributeCount = classObject.resolveAttributeCount();

      m_valueArray = (nAttributeCount == 0) ? EMPTY_ARRAY : new Object[nAttributeCount];
   }

   protected ObjectException()
   {
      this(ClassObject.getEnvironment().findClass(CLASS_SYMBOL));
   }
   
   // operations

   /**
    * Initializes the exception.
    * @see ScriptingError#ScriptingError(String, Object[], Throwable)
    */
   protected void init(String sErrCode, Object[] argArray, Throwable cause)
   {
      m_sErrCode = sErrCode;
      m_argArray = argArray;

      if (cause != null)
      {
         initCause(cause);
      }
   }

   /**
    * @see nexj.core.scripting.object.ObjectOriented#getClassObject()
    */
   public ClassObject getClassObject()
   {
      return m_class;
   }

   /**
    * @see nexj.core.scripting.object.ObjectOriented#initialize(Machine)
    */
   public void initialize(Machine machine)
   {
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount == 0)
      {
         throw new InvocationException("err.scripting.minArgCount",
            new Object[]{m_class.getName(),
               Primitive.ONE_INTEGER,
               Primitive.createInteger(nArgCount)});
      }

      Object selector = machine.getArg(0, nArgCount);

      if (!(selector instanceof Symbol))
      {
         throw new InvocationException("err.scripting.funCall");
      }

      machine.setArg(0, nArgCount, this);

      Function fun = m_class.resolveFunction((Symbol)selector, nArgCount - 1);

      if (fun != null)
      {
         return fun.invoke(nArgCount, machine);
      }

      throw new InvocationException("err.scripting.methodLookup",
         new Object[]{selector.toString(), Primitive.createInteger(nArgCount - 1), m_class.getName()});
   }

   /**
    * @see nexj.core.scripting.object.ObjectOriented#invokeBase(int, nexj.core.scripting.Machine)
    */
   public boolean invokeBase(int nArgCount, Machine machine)
   {
      if (nArgCount == 0)
      {
         throw new InvocationException("err.scripting.minArgCount",
            new Object[]{m_class.getName(),
               Primitive.ONE_INTEGER,
               Primitive.createInteger(nArgCount)});
      }

      Object selector = machine.getArg(0, nArgCount);

      if (!(selector instanceof Symbol))
      {
         throw new InvocationException("err.scripting.funCall");
      }

      machine.setArg(0, nArgCount, this);

      Function fun = m_class.resolveBaseFunction((Symbol)selector, nArgCount - 1);

      if (fun != null)
      {
         return fun.invoke(nArgCount, machine);
      }

      throw new InvocationException("err.scripting.baseMethodLookup",
         new Object[]{selector.toString(), Primitive.createInteger(nArgCount - 1), m_class.getName()});
   }

   /**
    * @see nexj.core.scripting.object.ObjectOriented#setValue(int, java.lang.Object, Machine)
    */
   public void setValue(int nOffset, Object value, Machine machine)
   {
      m_valueArray[nOffset] = value;
   }

   /**
    * @see nexj.core.scripting.object.ObjectOriented#getValue(int, Machine)
    */
   public Object getValue(int nOffset, Machine machine)
   {
      return m_valueArray[nOffset];
   }

   /**
    * Exposes class members from ObjectException.
    * @param classObject The destination class object.
    */
   public static void addClassMembers(ClassObject classObject)
   {
      classObject.addMethod("new", 2, true, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ObjectException e = (ObjectException)((ClassObject)machine.getArg(0, nArgCount)).createObject();
            Object err = machine.getArg(1, nArgCount);

            if (!(err instanceof String))
            {
               throw new TypeMismatchException("new");
            }

            Object[] argArray = null;
            Throwable cause = null;

            if (nArgCount > 2)
            {
               int nCount = nArgCount - 2;
               Object obj = machine.getArg(nCount + 1, nArgCount);

               if (obj instanceof Throwable)
               {
                  cause = (Throwable)obj;
                  --nCount;
               }

               if (nCount > 0)
               {
                  argArray = new Object[nCount];

                  for (int i = 2; i < nCount + 2; ++i)
                  {
                     argArray[i - 2] = machine.getArg(i, nArgCount);
                  }
               }
            }

            e.init((String)err, argArray, cause);
            machine.invoke(e, GenericObject.INITIALIZE_SYMBOL, (Object[])null);
            machine.returnValue(e, nArgCount);

            return false;
         }
      });
   }
}
