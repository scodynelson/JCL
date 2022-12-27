package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.ConstantDynamic;

@Getter
@RequiredArgsConstructor
public final class ConstantDynamicWrapper implements LispStruct {
	private final ConstantDynamic constantDynamic;
}