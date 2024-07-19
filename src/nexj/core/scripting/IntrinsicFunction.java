// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * Interface implemented by intrinsic functions.
 */
public interface IntrinsicFunction extends Function
{
   /**
    * @return The intrinsic function symbol.
    */
   Symbol getSymbol();
   
   /**
    * @return True if the execution is left to p-code.
    */
   boolean isPCode();
   
   /**
    * Optionally generates code for this intrinsic function.
    * @param buf The p-code output buffer.
    * @param instr The instruction for this function call.
    * @return True if the code has been generated,
    * false to make the caller generate default code.
    */
   boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr);
}
