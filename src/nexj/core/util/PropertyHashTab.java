package nexj.core.util;

/**
 * Helper class for implementing PropertyMap interface.
 * NOTE: Do not expose this type through public methods,
 * since it derives for efficiency from HashTab.
 */
public class PropertyHashTab extends HashTab implements PropertyMap
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -6845593387477998262L;

   /**
    * Empty iterator.
    */
   public final static PropertyIterator EMPTY_ITERATOR = new PropertyIterator()
   {
      public String getName()
      {
         throw new IllegalStateException("Cannot get name from an empty iterator");
      }

      public Object getValue()
      {
         throw new IllegalStateException("Cannot get value from an empty iterator");
      }

      public void setValue(Object value)
      {
         throw new IllegalStateException("Cannot set value on an empty iterator");
      }

      public void remove()
      {
         throw new IllegalStateException("Cannot remove from an empty iterator");
      }

      public boolean hasNext()
      {
         return false;
      }

      public Object next()
      {
         throw new java.util.NoSuchElementException("Cannot iterate with an empty iterator");
      }
   };

   // constructors

   public PropertyHashTab()
   {
      super();
   }

   public PropertyHashTab(int nCount)
   {
      super(nCount);
   }

   // operations

   /**
    * @see nexj.core.util.PropertyMap#setValue(java.lang.String, java.lang.Object)
    */
   public void setValue(String sName, Object value)
   {
      put(sName, value);
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String, java.lang.Object)
    */
   public Object findValue(String sName, Object defaultValue)
   {
      Object value = get(sName);

      if (value == null && defaultValue != null && !contains(sName))
      {
         return defaultValue;
      }

      return value;
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String)
    */
   public Object findValue(String sName)
   {
      return get(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#getValue(java.lang.String)
    */
   public Object getValue(String sName)
   {
      return get(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#hasValue(java.lang.String)
    */
   public boolean hasValue(String sName)
   {
      return contains(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#getValueCount()
    */
   public int getValueCount()
   {
      return size();
   }

   /**
    * @see nexj.core.util.PropertyMap#getIterator()
    */
   public PropertyIterator getIterator()
   {
      return new PropertyHashTabIterator();
   }

   /**
    * @see nexj.core.util.PropertyMap#getClassName()
    */
   public String getClassName()
   {
      return getClass().getName();
   }

   // inner classes

   protected class PropertyHashTabIterator extends GenericHashTabIterator implements PropertyIterator
   {
      /**
       * @see nexj.core.util.PropertyIterator#getName()
       */
      public String getName()
      {
         return (String)getKey();
      }
   }
}