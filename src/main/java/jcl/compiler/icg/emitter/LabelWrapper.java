package jcl.compiler.icg.emitter;

import jcl.lang.LispStruct;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.Label;

@Getter
@RequiredArgsConstructor
public final class LabelWrapper implements LispStruct {
	private final Label label;
}