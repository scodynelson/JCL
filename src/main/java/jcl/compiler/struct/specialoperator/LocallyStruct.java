/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class LocallyStruct extends CompilerSpecialOperatorStruct {

	private final List<SpecialDeclarationStruct> specials;
	private final PrognStruct forms;
	private final Environment locallyEnvironment;

	public LocallyStruct(final List<SpecialDeclarationStruct> specials, final List<LispStruct> forms,
	                     final Environment locallyEnvironment) {
		super("locally");
		this.specials = specials;
		this.forms = new PrognStruct(forms);
		this.locallyEnvironment = locallyEnvironment;
	}

	/**
	 * {@inheritDoc} Generation method for {@code LocallyStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Temporarily pushing the {@link LocallyStruct#locallyEnvironment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link LocallyStruct#forms} values</li>
	 * </ol>
	 * As an example, it will transform {@code (locally 1)} into the following Java code:
	 * <pre>
	 * {@code
	 *      BigInteger var2 = new BigInteger("1");
	 *      new IntegerStruct(var2);
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();
		final int environmentStore = methodBuilder.getEnvironmentStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.ENVIRONMENT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.ENVIRONMENT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.ENVIRONMENT_INIT_ENVIRONMENT_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, environmentStore);

		final Set<SymbolStruct> existingDynamicSymbols = new HashSet<>(generatorState.getDynamicSymbols());

		for (final SpecialDeclarationStruct special : specials) {
			generatorState.getDynamicSymbols().add(special.getVar());
		}

		forms.generate(generatorState);

		for (final SpecialDeclarationStruct special : specials) {
			final SymbolStruct var = special.getVar();
			if (!existingDynamicSymbols.contains(var)) {
				generatorState.getDynamicSymbols().remove(var);
			}
		}

		mv.visitInsn(Opcodes.ARETURN);
	}
}
