// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;


/**
 * A type definition.
 */
public abstract class Type extends NamedMetadataObject
{
   // constructors

   /**
    * Creates a type with a given name.
    * @param sName The type name.
    */
   public Type(String sName)
   {
      super(sName);
   }
   
   /**
    * Creates a type with a null name.
    */
   protected Type()
   {
      super();
   }

   // operations

   /**
    * Determines if the type is primitive.
    * @return true if primitive.
    */
   public abstract boolean isPrimitive();

   /**
    * @return The base type, or null if this is the root of a hierarchy.
    */
   public abstract Type getBaseType();

   /**
    * Converts a value to this type.
    * @param value The value to convert.
    * @return The converter value.
    * @throws TypeConversionException if the conversion fails.
    */
   public Object convert(Object value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      if (value instanceof Typed)
      {
         Type type = ((Typed)value).getType();

         if (type.isUpcast(this) || this.isUpcast(type))
         {
            return value;
         }
      }

      throw new TypeConversionException(this);
   }

   /**
    * Determines if a type can be upcast to obtain this type.
    * @param metaclass The type to upcast.
    * @return True if this class can be obtained by upcasting type.
    */
   public boolean isUpcast(Type type)
   {
      while (type != null)
      {
         if (getName().equals(type.getName()))
         {
            return isPrimitive() == type.isPrimitive();
         }

         type = type.getBaseType();
      }
      
      return false;
   }

   /**
    * Determines the type of a given value.
    * @param value The value of which to determine the type.
    * @return The value type (null if value is null).
    * @throws MetadataException if the type has not been recognized.
    */
   public static Type typeOf(Object value)
   {
      if (value instanceof Typed)
      {
         return ((Typed)value).getType();
      }

      return Primitive.primitiveOf(value);
   }
}
