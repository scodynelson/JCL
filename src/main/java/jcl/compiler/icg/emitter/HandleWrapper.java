package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.Handle;

@Getter
@RequiredArgsConstructor
public final class HandleWrapper implements LispStruct {
	private final Handle handle;
}