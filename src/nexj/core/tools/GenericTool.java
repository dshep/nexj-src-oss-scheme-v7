// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.PrintStream;
import java.util.Properties;

import nexj.core.util.Logger;
import nexj.core.util.SysUtil;

/**
 * Abstract command-line tool shell.
 */
public abstract class GenericTool
{
   // constants

   /**
    * Exit code if usage information is printed.
    */
   public final static int EXIT_USAGE = 1;

   /**
    * Exit code if an unhandled exception is caught. Indicates an unexpected error. 
    */
   public final static int EXIT_ERROR = 2;

   // attributes

   /**
    * The process exit code.
    */
   private int m_nExitCode;

   // associations

   /**
    * The configuration properties.
    */
   private Properties m_properties = SysUtil.getConfigProperties();

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericTool.class);

   // operations

   /**
    * Runs the generic tool with command-line arguments.
    * @param args The command-line argument array.
    */
   protected final void run(String[] args)
   {
      String[] optionUsageArray = getOptionUsage();
      
      if (optionUsageArray != null && optionUsageArray.length == 0)
      {
         optionUsageArray = null;
      }
      
      String[] commandUsageArray = getCommandUsage();

      if (commandUsageArray != null && commandUsageArray.length == 0)
      {
         commandUsageArray = null;
      }

      if ((commandUsageArray == null) != (args.length == 0) &&
         (isCommandRequired() && args.length == 0 || args.length != 0) ||
         args.length == 1 && (args[0].equals("/?") || args[0].equals("-?") || args[0].equals("--help")))
      {
         System.err.println("Usage: java [options] " + getClass().getName() +
            ((commandUsageArray != null) ? " [commands]" : ""));
         System.err.println("Options:");
         System.err.println("   -Dnexj.config=<config file name>");
         printUsage(System.err, optionUsageArray);
         
         if (commandUsageArray != null)
         {
            System.err.println("Commands:");
            printUsage(System.err, commandUsageArray);
         }

         System.exit(EXIT_USAGE);
         return;
      }

      try
      {
         begin();
         
         if (args.length == 0)
         {
            execute(null);
         }
         else
         {
            for (int i = 0; i < args.length; ++i)
            {
               if (s_logger.isInfoEnabled())
               {
                  s_logger.info("Executing command \"" + args[i] + "\"");
               }
               
               execute(args[i]);
            }
         }
         
         end();
      }
      catch (Throwable e)
      {
         s_logger.error("Error executing the tool command", e);
         
         String sMsg = e.getMessage();

         for (Throwable t = e.getCause(); t != null && (sMsg == null || sMsg.length() == 0); t = t.getCause())
         {
            try
            {
               sMsg = t.getMessage();
            }
            catch (Exception x)
            {
            }
         }

         if (sMsg == null || sMsg.length() == 0)
         {
            sMsg = e.getClass().getName();
         }

         System.err.println();
         System.err.println("Error: " + sMsg);
         setExitCode(EXIT_ERROR);
      }
      finally
      {
         dispose();
      }

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Exit code = " + getExitCode());
      }

      System.exit(getExitCode());
   }
   
   /**
    * Prints the usage array.
    * @param out The print stream where to print the array.
    * @param usageArray The usage array to print.
    */
   private void printUsage(PrintStream out, String[] usageArray)
   {
      if (usageArray == null)
      {
         return;
      }
      
      for (int i = 0; i < usageArray.length; ++i)
      {
         out.println("   " + usageArray[i]);
      }
   }
   
   /**
    * Begins the processing.
    */
   protected void begin() throws Exception
   {
   }
   
   /**
    * Executes the specified command.
    * @param sCommand The command to execute. 
    */
   protected abstract void execute(String sCommand) throws Exception;
   
   /**
    * Ends the processing.
    */
   protected void end() throws Exception
   {
   }
   
   /**
    * Releases the allocated system resources.
    */
   protected void dispose()
   {
   }

   /**
    * Sets the process exit code.
    * @param nExitCode The process exit code to set.
    */
   public void setExitCode(int nExitCode)
   {
      m_nExitCode = nExitCode;
   }

   /**
    * @return The process exit code.
    */
   public int getExitCode()
   {
      return m_nExitCode;
   }
   
   /**
    * @return An array of strings describing the usage of the command-line options. 
    */
   protected abstract String[] getOptionUsage();
   
   /**
    * @return An array of strings describing the usage of the commands.
    */
   protected abstract String[] getCommandUsage();
   
   /**
    * @return true if a command is required.
    */
   protected boolean isCommandRequired()
   {
      return getCommandUsage() != null;
   }
   
   /**
    * Gets a configuration property by name.
    * @param sName The property name.
    * @return The property value, or null if not found.
    */
   public final String getProperty(String sName)
   {
      String sProperty = System.getProperty(sName, m_properties.getProperty(sName));

      if (sProperty != null && sProperty.length() == 0)
      {
         sProperty = null;
      }

      return sProperty;
   }

   /**
    * Gets a configuration property by name.
    * @param sName The property name.
    * @param sDefaultValue The default property value.
    * @return The property value, or sDefaultValue if not found.
    */
   protected final String getProperty(String sName, String sDefaultValue)
   {
      String sProperty = System.getProperty(sName, m_properties.getProperty(sName, sDefaultValue));

      if (sProperty != null && sProperty.length() == 0)
      {
         sProperty = null;
      }

      return sProperty;
   }

   /**
    * Gets a required configuration property by name.
    * @param sName The property name.
    * @return The property value.
    * @throws IllegalArgumentException if the property was not found.
    */
   protected final String getRequiredProperty(String sName)
   {
      String sValue = System.getProperty(sName, m_properties.getProperty(sName));

      if (sValue == null || sValue.length() == 0)
      {
         throw new IllegalArgumentException("Missing required property \"" + sName + "\"");
      }

      return sValue;
   }

   /**
    * Gets a flag indicating whether or not a configuration property has been
    * specified.
    * @param sName The property name.
    * @return True if the property has been specified; false otherwise.
    */
   protected final boolean hasProperty(String sName)
   {
      return getProperty(sName, null) != null;
   }

   /**
    * Sets a tool property.
    * @param sName The property name.
    * @param sValue The property value.
    */
   public void setProperty(String sName, String sValue)
   {
      if (m_properties == SysUtil.getConfigProperties())
      {
         m_properties = new Properties(m_properties);
      }

      m_properties.setProperty(sName, sValue);
   }

   /**
    * Appends two string arrays.
    * @param leftArray The left array.
    * @param rightArray The right array.
    * @return The array containing leftArray elements followed by rightArray elements.
    */
   protected static String[] append(String[] leftArray, String[] rightArray)
   {
      if (leftArray == null)
      {
         return rightArray;
      }

      if (rightArray == null)
      {
         return leftArray;
      }

      String[] array = new String[leftArray.length + rightArray.length];

      System.arraycopy(leftArray, 0, array, 0, leftArray.length);
      System.arraycopy(rightArray, 0, array, leftArray.length, rightArray.length);

      return array;
   }
}
