// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface implemented by objects providing a logger.
 */
public interface LoggerHolder
{
   /**
    * @return The logger specific to this object.
    */
   Logger getLogger();
}
