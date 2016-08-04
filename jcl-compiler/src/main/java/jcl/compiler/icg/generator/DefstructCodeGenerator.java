/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.Deque;
import java.util.List;
import java.util.Map;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.defstruct.DefstructStruct;
import jcl.lang.StandardClassStruct;
import jcl.lang.StructureClassStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStructImpl;
import jcl.type.LispType;
import jcl.type.StructureObjectType;
import jcl.type.TypeBaseClass;
import jcl.type.TypeFactory;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
final class DefstructCodeGenerator implements CodeGenerator<DefstructStruct> {

	private static final String STRUCT_TYPES_PACKAGE = "jcl/structures/struct/types/";

	private static final String STRUCT_CLASSES_PACKAGE = "jcl/structures/struct/classes/";

	private static final String STRUCT_OBJECTS_PACKAGE = "jcl/structures/struct/objects/";

	private static final String STRUCTURE_TYPE_POSTFIX = "StructureType";

	private static final String STRUCTURE_CLASS_POSTFIX = "StructureClass";

	private static final String STRUCTURE_OBJECT_POSTFIX = "StructureObject";

	private static final String SYNTHETIC_INNER_CLASS_ID = "$1";

	private static final String SYNTHETIC_INNER_CLASS_DESC_PREFIX = "(L";

	private static final String SYNTHETIC_INNER_CLASS_DESC_POSTFIX = ";)V";

	private static final String STRUCTURE_TYPE_FACTORY_CLASS_SIMPLE_NAME = "Factory";

	private static final String STRUCTURE_TYPE_IMPL_POSTFIX = "StructureTypeImpl";

	private static final String INIT_SLOTS_MAP_METHOD_NAME = "initSlotsMap";

	private static final String INIT_SLOTS_MAP_METHOD_DESC = "()V";

	private static final String SLOTS_FIELD = "slots";

	private static final String STRUCTURE_TYPE_FACTORY_SIGNATURE_PREFIX = "Ljava/lang/Object;Ljcl/type/TypeFactory<L";

	private static final String STRUCTURE_TYPE_FACTORY_SIGNATURE_POSTFIX = ";>;";

	private static final String[] STRUCTURE_TYPE_FACTORY_INTERFACES = {GenerationConstants.TYPE_FACTORY_NAME};

	private static final String GET_INSTANCE_METHOD_NAME = "getInstance";

	private static final String GET_INSTANCE_BRIDGE_METHOD_DESC = "()Ljcl/type/LispType;";

	private static final String STRUCTURE_OBJECT_INIT_SCS_SS_SOS_METHOD_DESC = "(Ljcl/lang/StructureClassStruct;Ljcl/lang/SymbolStructImpl;Ljcl/lang/StructureObjectStruct;)V";

	private static final String STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_DESC = "(Ljcl/lang/SymbolStructImpl;Ljcl/lang/SymbolStructImpl;Ljava/util/List;Ljava/util/List;)V";

	private static final String STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_SIGNATURE = "(Ljcl/lang/SymbolStructImpl;Ljcl/lang/SymbolStructImpl;Ljava/util/List<Ljava/lang/Class<+Ljcl/lang/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/lang/LispStruct;>;>;)V";

	private static final String STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC = "(Ljcl/type/LispType;Ljcl/lang/SymbolStructImpl;Ljcl/lang/SymbolStructImpl;Ljava/util/List;Ljava/util/List;)V";

	private static final String STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_SIGNATURE = "(Ljcl/type/LispType;Ljcl/lang/SymbolStructImpl;Ljcl/lang/SymbolStructImpl;Ljava/util/List<Ljava/lang/Class<+Ljcl/lang/LispStruct;>;>;Ljava/util/List<Ljava/lang/Class<+Ljcl/lang/LispStruct;>;>;)V";

	private static final String STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME = "newInstance";

	private static final String STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC = "()Ljcl/lang/StructureObjectStruct;";

	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<DefstructStruct> event) {
		final DefstructStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final SymbolStructImpl structureSymbol = input.getStructureSymbol();
		final String structureName = structureSymbol.getName();

		final String systemTimePostfix = "_" + System.nanoTime();

		final String structureTypeClassName = STRUCT_TYPES_PACKAGE + structureName + STRUCTURE_TYPE_POSTFIX + systemTimePostfix;
		final String structureTypeClassDesc = 'L' + structureTypeClassName + ';';
		final String structureClassClassName = STRUCT_CLASSES_PACKAGE + structureName + STRUCTURE_CLASS_POSTFIX + systemTimePostfix;
		final String structureClassClassDesc = 'L' + structureClassClassName + ';';
		final String structureObjectClassName = STRUCT_OBJECTS_PACKAGE + structureName + STRUCTURE_OBJECT_POSTFIX + systemTimePostfix;

		final String structureTypeImplSyntheticClassName = structureTypeClassName + SYNTHETIC_INNER_CLASS_ID + systemTimePostfix;
		final String structureTypeImplSyntheticClassDesc = SYNTHETIC_INNER_CLASS_DESC_PREFIX + structureTypeImplSyntheticClassName + SYNTHETIC_INNER_CLASS_DESC_POSTFIX;
		final String structureTypeFactoryClassName = structureTypeClassName + '$' + STRUCTURE_TYPE_FACTORY_CLASS_SIMPLE_NAME + systemTimePostfix;

		final String structureTypeImplClassSimpleName = structureName + STRUCTURE_TYPE_IMPL_POSTFIX + systemTimePostfix;
		final String structureTypeImplClassName = structureTypeFactoryClassName + '$' + structureTypeImplClassSimpleName;

		generateStructureObject(input, generatorState,
		                        structureClassClassName,
		                        structureClassClassDesc,
		                        structureObjectClassName);
		generateStructureClass(input, generatorState,
		                       structureTypeClassName,
		                       structureTypeClassDesc,
		                       structureClassClassName,
		                       structureClassClassDesc,
		                       structureObjectClassName);
		generateStructureTypeImpl(generatorState,
		                          structureName,
		                          structureTypeClassName,
		                          structureTypeFactoryClassName,
		                          structureTypeImplClassSimpleName,
		                          structureTypeImplClassName,
		                          structureTypeImplSyntheticClassName,
		                          structureTypeImplSyntheticClassDesc);
		generateStructureTypeFactory(generatorState,
		                             structureTypeClassName,
		                             structureTypeClassDesc,
		                             structureTypeFactoryClassName,
		                             structureTypeImplClassSimpleName,
		                             structureTypeImplClassName);
		generateStructureType(input, generatorState,
		                      structureTypeClassName,
		                      structureTypeClassDesc,
		                      structureTypeFactoryClassName,
		                      structureTypeImplClassSimpleName,
		                      structureTypeImplClassName,
		                      structureTypeImplSyntheticClassName,
		                      structureTypeImplSyntheticClassDesc);

		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		if (!classBuilderDeque.isEmpty()) {
			final JavaMethodBuilder previousMethodBuilder = generatorState.getCurrentMethodBuilder();
			final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

			final int packageStore = previousMethodBuilder.getNextAvailableStore();
			final int symbolStore = previousMethodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(structureSymbol, generatorState, packageStore, symbolStore);

			previousMv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			previousMv.visitInsn(Opcodes.DUP); // DUP the symbol so it will still be on the stack after we set the structure class.

			previousMv.visitFieldInsn(Opcodes.GETSTATIC, structureClassClassName, GenerationConstants.SINGLETON_INSTANCE, structureClassClassDesc);
			previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                           GenerationConstants.SYMBOL_STRUCT_NAME,
			                           GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME,
			                           GenerationConstants.SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC,
			                           false);
		}
	}

	//	/**
//	 * TODO: finish javadoc
//	 * Private method for generating a new {@link StructureObjectType} class, by performing the following operations:
//	 * <ol>
//	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
//	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
//	 * {@link JavaClassBuilder#classWriter}</li>
//	 * <li>Visiting {@link TypeFactory} inner implementation class being generated as an inner class for the outer
//	 * top-level {@link StructureObjectType} class</li>
//	 * <li>Visiting {@link StructureObjectType} inner implementation class for the class being generated</li>
//	 * <li>Generating the code for the default constructor</li>
//	 * <li>Generating the code for the {@link TypeFactory#getInstance()} method</li>
//	 * <li>Generating the code for the {@code Bridge} {@link TypeFactory#getInstance()} method</li>
//	 * <li>Generating the code to end the new class visitation</li>
//	 * </ol>
//	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
//	 * code:
//	 * <pre>
//	 * {@code
//	 * package jcl.structures.struct.types;
//
//	import jcl.structures.struct.types.FOOStructureType_34553964509765.1_34553964509765;
//	import StructureObjectType;
//	import TypeBaseClass;
//	import TypeFactory;
//	import AtomicTypeSpecifier;
//	import org.apache.commons.lang3.builder.HashCodeBuilder;
//
//	public interface FOOStructureType_34553964509765 extends StructureObjectType {
//	FOOStructureType_34553964509765 INSTANCE = new FOOStructureType_34553964509765.Factory.FOOStructureTypeImpl_34553964509765((1_34553964509765)null);
//
//	public static class Factory implements TypeFactory<FOOStructureType_34553964509765> {
//	public Factory() {
//	}
//
//	public FOOStructureType_34553964509765 getInstance() {
//	return FOOStructureType_34553964509765.INSTANCE;
//	}
//
//	private static final class FOOStructureTypeImpl_34553964509765 extends TypeBaseClass implements FOOStructureType_34553964509765, AtomicTypeSpecifier {
//
//	private FOOStructureTypeImpl_34553964509765() {
//	super("FOO");
//	}
//
//	public int hashCode() {
//	return (new HashCodeBuilder()).appendSuper(super.hashCode()).toHashCode();
//	}
//
//	public boolean equals(Object var1) {
//	return this == var1 || var1 instanceof FOOStructureType_34553964509765;
//	}
//	}
//	}
//	}
//	 * }
//	 * </pre>
//	 * <pre>
//	 * {@code
//	 * package jcl.structures.struct.types;
//
//	import jcl.structures.struct.types.FOOStructureType_35638529195808;
//	import jcl.structures.struct.types.BARStructureType_35662194797508.1_35662194797508;
//	import TypeBaseClass;
//	import TypeFactory;
//	import AtomicTypeSpecifier;
//	import org.apache.commons.lang3.builder.HashCodeBuilder;
//
//	public interface BARStructureType_35662194797508 extends FOOStructureType_35638529195808 {
//	BARStructureType_35662194797508 INSTANCE = new BARStructureType_35662194797508.Factory.BARStructureTypeImpl_35662194797508((1_35662194797508)null);
//
//	public static class Factory implements TypeFactory<BARStructureType_35662194797508> {
//	public Factory() {
//	}
//
//	public BARStructureType_35662194797508 getInstance() {
//	return BARStructureType_35662194797508.INSTANCE;
//	}
//
//	private static final class BARStructureTypeImpl_35662194797508 extends TypeBaseClass implements BARStructureType_35662194797508, AtomicTypeSpecifier {
//
//	private BARStructureTypeImpl_35662194797508() {
//	super("BAR");
//	}
//
//	public int hashCode() {
//	return (new HashCodeBuilder()).appendSuper(super.hashCode()).toHashCode();
//	}
//
//	public boolean equals(Object var1) {
//	return this == var1 || var1 instanceof BARStructureType_35662194797508;
//	}
//	}
//	}
//	}
//	 * }
//	 * </pre>
//	 *
//	 * @param generatorState
//	 * 		stateful object used to hold the current state of the code generation process
//	 * @param structureTypeClassName
//	 * 		the {@link String} containing the name of the {@link StructureObjectType} whose {@link TypeFactory}
//	 * 		implementation class is being generated for
//	 * @param structureTypeClassDesc
//	 * 		the {@link String} containing the type descriptor of the {@link StructureObjectType} whose {@link TypeFactory}
//	 * 		implementation class is being generated for
//	 * @param structureTypeFactoryClassName
//	 * 		the {@link String} containing the name of the {@link TypeFactory} implementation class being generated
//	 * @param structureTypeImplClassSimpleName
//	 * 		the {@link String} containing the simple class name of the {@link StructureObjectType} implementation class
//	 * 		to be an inner class of the {@link TypeFactory} implementation class being generated
//	 * @param structureTypeImplClassName
//	 * 		the {@link String} containing the name of the {@link StructureObjectType} implementation class to be an inner
//	 * 		class of the {@link TypeFactory} implementation class being generated
//	 */
	private static void generateStructureType(final DefstructStruct input, final GeneratorState generatorState,
	                                          final String structureTypeClassName,
	                                          final String structureTypeClassDesc,
	                                          final String structureTypeFactoryClassName,
	                                          final String structureTypeImplClassSimpleName,
	                                          final String structureTypeImplClassName,
	                                          final String structureTypeImplSyntheticClassName,
	                                          final String structureTypeImplSyntheticClassDesc) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureTypeClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureTypeClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final String[] interfaces = getStructureTypeInterfaces(input);

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE,
		         structureTypeClassName,
		         null,
		         GenerationConstants.JAVA_OBJECT_NAME,
		         interfaces);

		cw.visitInnerClass(structureTypeImplSyntheticClassName,
		                   null,
		                   null,
		                   Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);
		cw.visitInnerClass(structureTypeFactoryClassName,
		                   structureTypeClassName,
		                   STRUCTURE_TYPE_FACTORY_CLASS_SIMPLE_NAME,
		                   Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplClassName,
		                   structureTypeFactoryClassName,
		                   structureTypeImplClassSimpleName,
		                   Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		CodeGenerators.generateSingletonInstanceField(cw, structureTypeClassDesc);
		generateStructureTypeClassInitMethod(generatorState, cw,
		                                     structureTypeClassName, structureTypeClassDesc,
		                                     structureTypeImplClassName, structureTypeImplSyntheticClassDesc);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for retrieving the interfaces array from the provided {@link DefstructStruct#includeStructureClass}
	 * based on its {@link Class#getInterfaces()} value. If the {@link DefstructStruct#includeStructureClass} is null,
	 * the default interface will be {@link StructureObjectType}. If the result of {@link Class#getInterfaces()} on the
	 * {@link DefstructStruct#includeStructureClass} value is an empty array, the default interface will be {@link
	 * StructureObjectType}.
	 *
	 * @param input
	 * 		the {@link DefstructStruct} to provided the possible included {@link StandardClassStruct} for the new {@link
	 * 		StructureObjectType} to be created
	 *
	 * @return an array containing the interfaces array for creating the new defstruct {@link StructureObjectType}
	 */
	private static String[] getStructureTypeInterfaces(final DefstructStruct input) {
		final StructureClassStruct includeStructureClass = input.getIncludeStructureClass();

		final String[] interfaces = new String[1];
		if (includeStructureClass == null) {
			interfaces[0] = Type.getInternalName(StructureObjectType.class);
		} else {
			final LispType includeStructureClassType = includeStructureClass.getType();
			final Class<?> includeStructureClassTypeClass = includeStructureClassType.getClass();
			final Class<?>[] includeStructureClassTypeInterfaces = includeStructureClassTypeClass.getInterfaces();
			if (includeStructureClassTypeInterfaces.length == 0) {
				interfaces[0] = Type.getInternalName(StructureObjectType.class);
			} else {
				// NOTE: Because of the way we build these structure types, the first interface will be the one we want.
				final Class<?> includeStructureClassTypeInterface = includeStructureClassTypeInterfaces[0];
				final String includeStructureClassTypeName = Type.getInternalName(includeStructureClassTypeInterface);

				interfaces[0] = includeStructureClassTypeName;
			}
		}
		return interfaces;
	}

	/**
	 * Private method for generating the {@link StructureObjectType} class level initialization method ({@code clinit})
	 * for the generated {@link StructureObjectType} being written to via the provided {@link ClassWriter}. The
	 * generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to create a new instance of the {@link StructureObjectType} implementation by invoking
	 * the implementation {@code Synthetic} constructor with a {@code null} argument value and storing it into the
	 * singleton {@code INSTANCE} field</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 *      FOOStructureType_1 INSTANCE = new FOOStructureType_1.Factory.FOOStructureTypeImpl_1((1_1)null);
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} for this class initialization
	 * 		method
	 * @param structureTypeClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureObjectType} for this class
	 * 		initialization method
	 * @param structureTypeImplClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} implementation class for
	 * 		initializing the singleton {@code INSTANCE} field
	 * @param structureTypeImplSyntheticClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureObjectType} {@code Synthetic}
	 * 		implementation class for creating the singleton {@code INSTANCE} field
	 */
	private static void generateStructureTypeClassInitMethod(final GeneratorState generatorState, final ClassWriter cw,
	                                                         final String structureTypeClassName,
	                                                         final String structureTypeClassDesc,
	                                                         final String structureTypeImplClassName,
	                                                         final String structureTypeImplSyntheticClassDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
		                                        GenerationConstants.CLASS_INIT_METHOD_NAME,
		                                        GenerationConstants.CLASS_INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureTypeImplClassName);
		mv.visitInsn(Opcodes.DUP);
		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureTypeImplClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   structureTypeImplSyntheticClassDesc,
		                   false);
		mv.visitFieldInsn(Opcodes.PUTSTATIC,
		                  structureTypeClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureTypeClassDesc);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating a new {@link StructureObjectType} {@link TypeFactory} class, by performing the
	 * following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Visiting {@link TypeFactory} inner implementation class being generated as an inner class for the outer
	 * top-level {@link StructureObjectType} class</li>
	 * <li>Visiting {@link StructureObjectType} inner implementation class for the class being generated</li>
	 * <li>Generating the code for the default constructor</li>
	 * <li>Generating the code for the {@link TypeFactory#getInstance()} method</li>
	 * <li>Generating the code for the {@code Bridge} {@link TypeFactory#getInstance()} method</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * </ol>
	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * public static class Factory implements TypeFactory<FOOStructureType1> {
	 *      public Factory() {
	 *      }
	 *
	 *      public FOOStructureType_1 getInstance() {
	 *          return FOOStructureType_1.INSTANCE;
	 *      }
	 *
	 *      private static final class FOOStructureTypeImpl_1 extends TypeBaseClass
	 *                                                        implements FOOStructureType_1, AtomicTypeSpecifier {
	 *
	 *          private FOOStructureTypeImpl_1() {
	 *              super("FOO");
	 *          }
	 *
	 *          public int hashCode() {
	 *              return (new HashCodeBuilder()).appendSuper(super.hashCode()).toHashCode();
	 *          }
	 *
	 *          public boolean equals(Object var1) {
	 *              return this == var1 || var1 instanceof FOOStructureType_1;
	 *          }
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} whose {@link TypeFactory}
	 * 		implementation class is being generated for
	 * @param structureTypeClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureObjectType} whose {@link TypeFactory}
	 * 		implementation class is being generated for
	 * @param structureTypeFactoryClassName
	 * 		the {@link String} containing the name of the {@link TypeFactory} implementation class being generated
	 * @param structureTypeImplClassSimpleName
	 * 		the {@link String} containing the simple class name of the {@link StructureObjectType} implementation class
	 * 		to be an inner class of the {@link TypeFactory} implementation class being generated
	 * @param structureTypeImplClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} implementation class to be an inner
	 * 		class of the {@link TypeFactory} implementation class being generated
	 */
	private static void generateStructureTypeFactory(final GeneratorState generatorState,
	                                                 final String structureTypeClassName,
	                                                 final String structureTypeClassDesc,
	                                                 final String structureTypeFactoryClassName,
	                                                 final String structureTypeImplClassSimpleName,
	                                                 final String structureTypeImplClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureTypeFactoryClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureTypeFactoryClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
		         structureTypeFactoryClassName,
		         STRUCTURE_TYPE_FACTORY_SIGNATURE_PREFIX + structureTypeClassName + STRUCTURE_TYPE_FACTORY_SIGNATURE_POSTFIX,
		         GenerationConstants.JAVA_OBJECT_NAME,
		         STRUCTURE_TYPE_FACTORY_INTERFACES);

		cw.visitInnerClass(structureTypeFactoryClassName,
		                   structureTypeClassName,
		                   STRUCTURE_TYPE_FACTORY_CLASS_SIMPLE_NAME,
		                   Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplClassName,
		                   structureTypeFactoryClassName,
		                   structureTypeImplClassSimpleName,
		                   Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		final String structureTypeGetInstanceDesc = "()L" + structureTypeClassName + ';';

		generateStructureTypeFactoryConstructor(generatorState, cw);
		generateStructureTypeFactoryGetInstanceMethod(generatorState, cw,
		                                              structureTypeClassName, structureTypeClassDesc, structureTypeGetInstanceDesc);
		generateStructureTypeFactoryGetInstanceBridgeMethod(generatorState, cw,
		                                                    structureTypeFactoryClassName, structureTypeGetInstanceDesc);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the constructor for the generated {@link StructureObjectType} {@link TypeFactory}
	 * being written to via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to invoke the {@link Object#Object()} super constructor</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * public Factory() {
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 */
	private static void generateStructureTypeFactoryConstructor(final GeneratorState generatorState,
	                                                            final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        GenerationConstants.INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_OBJECT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.INIT_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link TypeFactory#getInstance()} method for the generated {@link
	 * StructureObjectType} {@link TypeFactory} being written to via the provided {@link ClassWriter}. The generation
	 * will perform the following operations:
	 * <ol>
	 * <li>Generating the code to retrieve and return the {@link StructureObjectType} singleton {@code INSTANCE}
	 * field</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * public FOOStructureType_1 getInstance() {
	 *      return FOOStructureType_1.INSTANCE;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} whose instance will be returned from
	 * 		the {@link TypeFactory#getInstance()} method being generated
	 * @param structureTypeClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureObjectType} whose instance will be
	 * 		returned from the {@link TypeFactory#getInstance()} method being generated
	 * @param structureTypeGetInstanceDesc
	 * 		the {@link String} containing the type descriptor of the {@link TypeFactory#getInstance()} method being
	 * 		generated
	 */
	private static void generateStructureTypeFactoryGetInstanceMethod(final GeneratorState generatorState,
	                                                                  final ClassWriter cw,
	                                                                  final String structureTypeClassName,
	                                                                  final String structureTypeClassDesc,
	                                                                  final String structureTypeGetInstanceDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GET_INSTANCE_METHOD_NAME,
		                                        structureTypeGetInstanceDesc,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  structureTypeClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureTypeClassDesc);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link TypeFactory#getInstance()} {@code Bridge} method for the generated
	 * {@link StructureObjectType} {@link TypeFactory} being written to via the provided {@link ClassWriter}. The
	 * generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to call the actual {@link TypeFactory#getInstance()} method with the type descriptor of
	 * the provided {@code structureTypeGetInstanceDesc}</li>
	 * </ol>
	 * NOTE: This {@code Bridge} method allows the correct call to the {@link TypeFactory#getInstance()} method whose
	 * return type is a generic argument. Since generics have type erasure, a bridge method is needed to invoke the
	 * method with the correct return type specified by the generic argument.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureTypeFactoryClassName
	 * 		the {@link String} containing the name of the {@link TypeFactory} class being generated
	 * @param structureTypeGetInstanceDesc
	 * 		the {@link String} containing the type descriptor of the {@link TypeFactory#getInstance()} method the {@code
	 * 		Bridge} method will call
	 */
	private static void generateStructureTypeFactoryGetInstanceBridgeMethod(final GeneratorState generatorState,
	                                                                        final ClassWriter cw,
	                                                                        final String structureTypeFactoryClassName,
	                                                                        final String structureTypeGetInstanceDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_BRIDGE + Opcodes.ACC_SYNTHETIC,
		                                        GET_INSTANCE_METHOD_NAME,
		                                        GET_INSTANCE_BRIDGE_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   structureTypeFactoryClassName,
		                   GET_INSTANCE_METHOD_NAME,
		                   structureTypeGetInstanceDesc,
		                   false);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating a new {@link StructureObjectType} implementation class, by performing the
	 * following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Visiting {@link TypeFactory} inner implementation class for the {@link StructureObjectType} containing the
	 * {@link StructureObjectType} implementation class being generated</li>
	 * <li>Visiting {@link StructureObjectType} inner implementation class for the class being generated</li>
	 * <li>Visiting {@code Synthetic} {@link StructureObjectType} inner implementation class for the class being
	 * generated</li>
	 * <li>Generating the code for the default constructor</li>
	 * <li>Generating the code for the {@code Synthetic} constructor</li>
	 * <li>Generating the code for the {@link Object#hashCode()} method</li>
	 * <li>Generating the code for the {@link Object#equals(Object)} method</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * </ol>
	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * private static final class FOOStructureTypeImpl_1 extends TypeBaseClass
	 *                                                   implements FOOStructureType_1, AtomicTypeSpecifier {
	 *
	 *      private FOOStructureTypeImpl_1() {
	 *          super("FOO");
	 *      }
	 *
	 *      public int hashCode() {
	 *          return (new HashCodeBuilder()).appendSuper(super.hashCode()).toHashCode();
	 *      }
	 *
	 *      public boolean equals(Object var1) {
	 *          return this == var1 || var1 instanceof FOOStructureType_1;
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param structureName
	 * 		the {@link String} containing the name of the structure being created
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} whose implementation class is being
	 * 		generated for
	 * @param structureTypeFactoryClassName
	 * 		the {@link String} containing the name of the {@link TypeFactory} implementation class the outer top-level
	 * 		class containing the {@link StructureObjectType} implementation class being generated
	 * @param structureTypeImplClassSimpleName
	 * 		the {@link String} containing the simple class name of the {@link StructureObjectType} implementation class
	 * 		being generated
	 * @param structureTypeImplClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} implementation class being generated
	 * @param structureTypeImplSyntheticClassName
	 * 		the {@link String} containing the name of the {@code Synthetic} class for the generated {@link
	 * 		StructureObjectType} implementation
	 * @param structureTypeImplSyntheticClassDesc
	 * 		the {@link String} containing the type descriptor of the {@code Synthetic} class for the generated
	 * 		{@link StructureObjectType} implementation
	 */
	private static void generateStructureTypeImpl(final GeneratorState generatorState,
	                                              final String structureName,
	                                              final String structureTypeClassName,
	                                              final String structureTypeFactoryClassName,
	                                              final String structureTypeImplClassSimpleName,
	                                              final String structureTypeImplClassName,
	                                              final String structureTypeImplSyntheticClassName,
	                                              final String structureTypeImplSyntheticClassDesc) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureTypeImplClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureTypeImplClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_FINAL + Opcodes.ACC_SUPER,
		         structureTypeImplClassName,
		         null,
		         GenerationConstants.TYPE_BASE_CLASS_NAME,
		         new String[]{structureTypeClassName, GenerationConstants.ATOMIC_TYPE_SPECIFIER_NAME});

		cw.visitInnerClass(structureTypeFactoryClassName,
		                   structureTypeClassName,
		                   STRUCTURE_TYPE_FACTORY_CLASS_SIMPLE_NAME,
		                   Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplClassName,
		                   structureTypeFactoryClassName,
		                   structureTypeImplClassSimpleName,
		                   Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC);
		cw.visitInnerClass(structureTypeImplSyntheticClassName,
		                   null,
		                   null,
		                   Opcodes.ACC_STATIC + Opcodes.ACC_SYNTHETIC);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		generateStructureTypeImplConstructor(generatorState, cw,
		                                     structureName);
		generateStructureTypeImplSyntheticConstructor(generatorState, cw,
		                                              structureTypeImplClassName, structureTypeImplSyntheticClassDesc);
		generateStructureTypeImplHashCodeMethod(generatorState, cw);
		generateStructureTypeImplEqualsMethod(generatorState, cw,
		                                      structureTypeClassName);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the default constructor for the generated {@link StructureObjectType}
	 * implementation being written to via the provided {@link ClassWriter}. The generation will perform the following
	 * operations:
	 * <ol>
	 * <li>Generating the call to the {@link TypeBaseClass#TypeBaseClass(String)} super constructor, passing the
	 * provided {@code structureName} as the name of the type</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * private FOOStructureTypeImpl_1() {
	 *      super("FOO");
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param structureName
	 * 		the {@link String} containing the name of the structure being created
	 */
	private static void generateStructureTypeImplConstructor(final GeneratorState generatorState,
	                                                         final ClassWriter cw,
	                                                         final String structureName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        GenerationConstants.INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitLdcInsn(structureName);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.TYPE_BASE_CLASS_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.TYPE_BASE_CLASS_INIT_STRING_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@code Synthetic} constructor for the generated {@link StructureObjectType}
	 * implementation being written to via the provided {@link ClassWriter}. The generation will perform the following
	 * operations:
	 * <ol>
	 * <li>Generating the code to call the constructor of the {@link StructureObjectType} implementation object with
	 * the name of the provided {@code structureTypeImplClassSimpleName}</li>
	 * </ol>
	 * NOTE: This {@code Synthetic} constructor allows the outer top-level class to access the {@code private} {@link
	 * StructureObjectType} implementation constructor
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param structureTypeImplClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} implementation inner class the
	 * 		{@code Synthetic} class is generated for
	 * @param structureTypeImplSyntheticClassDesc
	 * 		the {@link String} containing the type descriptor of the {@code Synthetic} class for the generated
	 * 		{@link StructureObjectType} implementation
	 */
	private static void generateStructureTypeImplSyntheticConstructor(final GeneratorState generatorState,
	                                                                  final ClassWriter cw,
	                                                                  final String structureTypeImplClassName,
	                                                                  final String structureTypeImplSyntheticClassDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_SYNTHETIC,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        structureTypeImplSyntheticClassDesc,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureTypeImplClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.INIT_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link Object#hashCode()} method for the generated {@link StructureObjectType}
	 * being written to via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating to code to create a new {@link HashCodeBuilder}</li>
	 * <li>Generating the code to call {@link HashCodeBuilder#appendSuper(int)} with the value of {@code
	 * super.hashCode()}</li>
	 * <li>Generating the code to call and return the value of {@link HashCodeBuilder#toHashCode()}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * public int hashCode() {
	 *      return (new HashCodeBuilder()).appendSuper(super.hashCode()).toHashCode();
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 */
	private static void generateStructureTypeImplHashCodeMethod(final GeneratorState generatorState,
	                                                            final ClassWriter cw) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.JAVA_HASH_CODE_METHOD_NAME,
		                                        GenerationConstants.JAVA_HASH_CODE_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.HASH_CODE_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.HASH_CODE_BUILDER_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.INIT_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.TYPE_BASE_CLASS_NAME,
		                   GenerationConstants.JAVA_HASH_CODE_METHOD_NAME,
		                   GenerationConstants.JAVA_HASH_CODE_METHOD_DESC,
		                   false);

		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.HASH_CODE_BUILDER_NAME,
		                   GenerationConstants.HASH_CODE_BUILDER_APPEND_SUPER_METHOD_NAME,
		                   GenerationConstants.HASH_CODE_BUILDER_APPEND_SUPER_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.HASH_CODE_BUILDER_NAME,
		                   GenerationConstants.HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_NAME,
		                   GenerationConstants.HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.IRETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link Object#equals(Object)} method for the generated {@link
	 * StructureObjectType} being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Generating to code to load and compare the {@link StructureObjectType} instance with the {@link Object}
	 * parameter value for object instance equality</li>
	 * <li>Generating to code to load and compare the {@link StructureObjectType} instance for 'instanceOf' equality to
	 * the provided {@code structureTypeClassName}</li>
	 * <li>Generating the code to return {@code true} if either of the previous conditions are {@code true}, or {@code
	 * false} otherwise</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * public boolean equals(Object var1) {
	 *      return this == var1 || var1 instanceof FOOStructureType_1;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} whose implementation class this
	 * 		{@link Object#equals(Object)} method is being generated for
	 */
	private static void generateStructureTypeImplEqualsMethod(final GeneratorState generatorState,
	                                                          final ClassWriter cw,
	                                                          final String structureTypeClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.JAVA_EQUALS_METHOD_NAME,
		                                        GenerationConstants.JAVA_EQUALS_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int objStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, objStore);

		final Label thisEqObject = new Label();
		final Label objNotInstanceOfType = new Label();
		final Label endOfEqAndInstanceCheck = new Label();

		mv.visitJumpInsn(Opcodes.IF_ACMPEQ, thisEqObject);

		mv.visitVarInsn(Opcodes.ALOAD, objStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, structureTypeClassName);

		mv.visitJumpInsn(Opcodes.IFEQ, objNotInstanceOfType);

		mv.visitLabel(thisEqObject);
		mv.visitInsn(Opcodes.ICONST_1);
		mv.visitJumpInsn(Opcodes.GOTO, endOfEqAndInstanceCheck);

		mv.visitLabel(objNotInstanceOfType);
		mv.visitInsn(Opcodes.ICONST_0);

		mv.visitLabel(endOfEqAndInstanceCheck);

		mv.visitInsn(Opcodes.IRETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating a new {@link StructureClassStruct}, by performing the following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Generating the code for the {@code INSTANCE} singleton field</li>
	 * <li>Generating the code for the standard argument constructor</li>
	 * <li>Generating the code for the type addition argument constructor</li>
	 * <li>Generating the code for the {@link StructureClassStruct#newInstance()} method</li>
	 * <li>Generating the code for the class level initialization method ({@code clinit})</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * </ol>
	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * package jcl.structures.struct.classes;
	 *
	 * import java.util.List;
	 * import LispStruct;
	 * import LispType;
	 * import PackageStruct;
	 * import StructureClassStruct;
	 * import StructureObjectStruct;
	 * import jcl.structures.struct.objects.FOOStructureObject_1;
	 * import jcl.structures.struct.types.FOOStructureType_1;
	 * import SymbolStruct;
	 *
	 * public class FOOStructureClass_1 extends StructureClassStruct {
	 *      public static final FOOStructureClass_1 INSTANCE;
	 *
	 *      protected FOOStructureClass_1(SymbolStruct var1, SymbolStruct var2,
	 *                                    List<Class<? extends LispStruct>> var3,
	 *                                    List<Class<? extends LispStruct>> var4) {
	 *          this(FOOStructureType_1.INSTANCE, var1, var2, var3, var4);
	 *      }
	 *
	 *      protected FOOStructureClass_1(LispType var1, SymbolStruct var2, SymbolStruct var3,
	 *                                    List<Class<? extends LispStruct>> var4,
	 *                                    List<Class<? extends LispStruct>> var5) {
	 *          super(var1, var2, var3, var4, var5);
	 *      }
	 *
	 *      public StructureObjectStruct newInstance() {
	 *          return new FOOStructureObject_1();
	 *      }
	 *
	 *      static {
	 *          PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var2 = var1.intern("MAKE-FOO").getSymbol();
	 *          INSTANCE = new FOOStructureClass_1(var2, (SymbolStruct)null, (List)null, (List)null);
	 *      }
	 * }
	 * }
	 * </pre>
	 * NOTE: It is possible the generated class will not extend {@link StructureClassStruct}, if there is an included
	 * structure in the defstruct operation.
	 * As an example, it will transform {@code (compiler:%defstruct bar foo make-bar nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * package jcl.structures.struct.classes;
	 *
	 * import java.util.List;
	 * import LispStruct;
	 * import LispType;
	 * import PackageStruct;
	 * import StructureObjectStruct;
	 * import jcl.structures.struct.classes.FOOStructureClass_1;
	 * import jcl.structures.struct.objects.BARStructureObject_1;
	 * import jcl.structures.struct.types.BARStructureType_1;
	 * import SymbolStruct;
	 *
	 * public class BARStructureClass_1 extends FOOStructureClass_1 {
	 *      public static final BARStructureClass_1 INSTANCE;
	 *
	 *      protected BARStructureClass_1(SymbolStruct var1, SymbolStruct var2,
	 *                                    List<Class<? extends LispStruct>> var3,
	 *                                    List<Class<? extends LispStruct>> var4) {
	 *          this(BARStructureType_1.INSTANCE, var1, var2, var3, var4);
	 *      }
	 *
	 *      protected BARStructureClass_1(LispType var1, SymbolStruct var2, SymbolStruct var3,
	 *                                    List<Class<? extends LispStruct>> var4,
	 *                                    List<Class<? extends LispStruct>> var5) {
	 *          super(var1, var2, var3, var4, var5);
	 *      }
	 *
	 *      public StructureObjectStruct newInstance() {
	 *          return new BARStructureObject_1();
	 *      }
	 *
	 *      static {
	 *          PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var2 = var1.intern("MAKE-BAR").getSymbol();
	 *          INSTANCE = new BARStructureClass_1(var2, (SymbolStruct)null, (List)null, (List)null);
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link DefstructStruct} containing the {@link StructureObjectStruct} metadata needed for generating a new
	 * 		{@link StructureObjectStruct}
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureTypeClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the constructor code
	 * 		for
	 */
	private static void generateStructureClass(final DefstructStruct input, final GeneratorState generatorState,
	                                           final String structureTypeClassName,
	                                           final String structureTypeClassDesc,
	                                           final String structureClassClassName,
	                                           final String structureClassClassDesc,
	                                           final String structureObjectClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureClassClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureClassClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		final StructureClassStruct includeStructureClass = input.getIncludeStructureClass();
		final String includeStructureClassFileName;
		if (includeStructureClass == null) {
			includeStructureClassFileName = Type.getInternalName(StructureClassStruct.class);
		} else {
			includeStructureClassFileName = Type.getInternalName(includeStructureClass.getClass());
		}

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
		         structureClassClassName,
		         null,
		         includeStructureClassFileName,
		         null);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		CodeGenerators.generateSingletonInstanceField(cw, structureClassClassDesc);
		generateStructureClassConstructor(generatorState, cw,
		                                  structureTypeClassName, structureTypeClassDesc, structureClassClassName);
		generateStructureClassTypeArgConstructor(generatorState, cw,
		                                         includeStructureClassFileName);
		generateStructureClassNewInstanceMethod(generatorState, cw,
		                                        structureObjectClassName);
		generateStructureClassClassInitMethod(input, generatorState, cw,
		                                      structureClassClassName, structureClassClassDesc);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct#StructureClassStruct(SymbolStructImpl, SymbolStructImpl,
	 * List, List)} constructor for the generated {@link StructureClassStruct} being written to via the provided {@link
	 * ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the current class {@link StructureClassStruct#StructureClassStruct(LispType,
	 * SymbolStructImpl, SymbolStructImpl, List, List)}, passing the singleton {@link StructureObjectType} with the class name
	 * of the provided {@code structureTypeClassName}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * protected FOOStructureClass_1(SymbolStruct var1, SymbolStruct var2,
	 *                               List<Class<? extends LispStruct>> var3,
	 *                               List<Class<? extends LispStruct>> var4) {
	 *      this(FOOStructureType_1.INSTANCE, var1, var2, var3, var4);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param structureTypeClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectType} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureTypeClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureObjectType} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} to generate the constructor code
	 * 		for
	 */
	private static void generateStructureClassConstructor(final GeneratorState generatorState,
	                                                      final ClassWriter cw,
	                                                      final String structureTypeClassName,
	                                                      final String structureTypeClassDesc,
	                                                      final String structureClassClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_DESC,
		                                        STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_SIGNATURE,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int defaultConstructorSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int printerSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int directSuperClassesArgStore = methodBuilder.getNextAvailableStore();
		final int subClassesArgStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  structureTypeClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureTypeClassDesc);
		mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureClassClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct#StructureClassStruct(LispType, SymbolStructImpl,
	 * SymbolStructImpl, List, List)} constructor for the generated {@link StructureClassStruct} being written to via the
	 * provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the call to the super class {@link StructureClassStruct#StructureClassStruct(LispType,
	 * SymbolStructImpl, SymbolStructImpl, List, List)}, passing the singleton {@link StructureObjectType} with the class name
	 * of the provided {@code structureTypeClassName}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * protected FOOStructureClass_1(LispType var1, SymbolStruct var2, SymbolStruct var3,
	 *                               List<Class<? extends LispStruct>> var4,
	 *                               List<Class<? extends LispStruct>> var5) {
	 *      super(var1, var2, var3, var4, var5);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param includeStructureClassFileName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureClassStruct} super class, whether {@link StructureClassStruct} itself or another subclass {@link
	 * 		StructureClassStruct} implementation
	 */
	private static void generateStructureClassTypeArgConstructor(final GeneratorState generatorState,
	                                                             final ClassWriter cw,
	                                                             final String includeStructureClassFileName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC,
		                                        STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_SIGNATURE,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int typeArgStore = methodBuilder.getNextAvailableStore();
		final int defaultConstructorSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int printerSymbolArgStore = methodBuilder.getNextAvailableStore();
		final int directSuperClassesArgStore = methodBuilder.getNextAvailableStore();
		final int subClassesArgStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitVarInsn(Opcodes.ALOAD, typeArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, defaultConstructorSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, printerSymbolArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, directSuperClassesArgStore);
		mv.visitVarInsn(Opcodes.ALOAD, subClassesArgStore);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   includeStructureClassFileName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_CLASS_INIT_LISP_TYPE_SS_SS_LIST_LIST_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct#newInstance()} method for the generated {@link
	 * StructureClassStruct} being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Generating the code to create and return a new instance of the {@link StructureObjectStruct} with the
	 * provided {@code structureObjectClassName} class name</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * public StructureObjectStruct newInstance() {
	 *      return new FOOStructureObject_1();
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to be created and returned from
	 * 		the {@link StructureClassStruct#newInstance()} method
	 */
	private static void generateStructureClassNewInstanceMethod(final GeneratorState generatorState,
	                                                            final ClassWriter cw,
	                                                            final String structureObjectClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME,
		                                        STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureObjectClassName);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureObjectClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.INIT_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@link StructureClassStruct} class level initialization method ({@code
	 * clinit}) for the generated {@link StructureClassStruct} being written to via the provided {@link ClassWriter}.
	 * The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to fetch the {@link SymbolStructImpl} for the {@link DefstructStruct#defaultConstructorSymbol}
	 * if it is not {@code null}, or generating {@code null}</li>
	 * <li>Generating the code to fetch the {@link SymbolStructImpl} for the {@link DefstructStruct#printerSymbol} if it is
	 * not {@code null}, or generating {@code null}</li>
	 * <li>Generating null for both the 'directSuperClasses' and 'subClasses' {@link List}s</li>
	 * <li>Generating the code to create a new instance of the {@link StructureClassStruct} with the provided {@code
	 * structureClassClassName} and storing it into the singleton {@code INSTANCE} field</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * static {
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("MAKE-FOO").getSymbol();
	 *      INSTANCE = new FOOStructureClass_1(var2, (SymbolStruct)null, (List)null, (List)null);
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link DefstructStruct} containing the {@link StructureObjectStruct} metadata needed for generating a new
	 * 		{@link StructureObjectStruct}
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this class initialization
	 * 		method
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this class
	 * 		initialization method
	 */
	private static void generateStructureClassClassInitMethod(final DefstructStruct input, final GeneratorState generatorState,
	                                                          final ClassWriter cw,
	                                                          final String structureClassClassName,
	                                                          final String structureClassClassDesc) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC,
		                                        GenerationConstants.CLASS_INIT_METHOD_NAME,
		                                        GenerationConstants.CLASS_INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		@SuppressWarnings({"unused", "SuppressionAnnotation"})
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitTypeInsn(Opcodes.NEW, structureClassClassName);
		mv.visitInsn(Opcodes.DUP);

		final SymbolStructImpl defaultConstructorSymbol = input.getDefaultConstructorSymbol();
		if (defaultConstructorSymbol == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(defaultConstructorSymbol, generatorState, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
		}

		final SymbolStructImpl printerSymbol = input.getPrinterSymbol();
		if (printerSymbol == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final int packageStore = methodBuilder.getNextAvailableStore();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(printerSymbol, generatorState, packageStore, symbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
		}

		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitInsn(Opcodes.ACONST_NULL);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   structureClassClassName,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_CLASS_INIT_SS_SS_LIST_LIST_METHOD_DESC,
		                   false);
		mv.visitFieldInsn(Opcodes.PUTSTATIC,
		                  structureClassClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureClassClassDesc);

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating a new {@link StructureObjectStruct}, by performing the following operations:
	 * <ol>
	 * <li>Creating a new {@link JavaClassBuilder}, which internally creates a new {@link ClassWriter}</li>
	 * <li>Visiting a new class via {@link ClassWriter#visit(int, int, String, String, String, String[])} of the new
	 * {@link JavaClassBuilder#classWriter}</li>
	 * <li>Generating the code for the no-argument constructor</li>
	 * <li>Generating the code for the {@code initSlots} method</li>
	 * <li>Generating the code to end the new class visitation</li>
	 * </ol>
	 * As an example, it will transform {@code (compiler:%defstruct foo nil make-foo nil a)} into the following Java
	 * code:
	 * <pre>
	 * {@code
	 * package jcl.structures.struct.objects;
	 *
	 * import java.util.Map;
	 * import PackageStruct;
	 * import StructureObjectStruct;
	 * import jcl.structures.struct.classes.FOOStructureClass_1;
	 * import SymbolStruct;
	 *
	 * public class FOOStructureObject_1 extends StructureObjectStruct {
	 *
	 *      public FOOStructureObject_1() {
	 *          FOOStructureClass_1 var10001 = FOOStructureClass_1.INSTANCE;
	 *          PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var2 = var1.intern("FOO").getSymbol();
	 *          super(var10001, var2, (StructureObjectStruct)null);
	 *          this.initSlotsMap();
	 *      }
	 *
	 *      private void initSlotsMap() {
	 *          Map var1 = this.slots;
	 *          PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var3 = var2.intern("A").getSymbol();
	 *          var1.put(var3, (Object)null);
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link DefstructStruct} containing the {@link StructureObjectStruct} metadata needed for generating a new
	 * 		{@link StructureObjectStruct}
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the constructor code
	 * 		for
	 */
	private static void generateStructureObject(final DefstructStruct input, final GeneratorState generatorState,
	                                            final String structureClassClassName,
	                                            final String structureClassClassDesc,
	                                            final String structureObjectClassName) {

		final String fileName = CodeGenerators.getFileNameFromClassName(structureObjectClassName);

		final JavaClassBuilder currentClass = new JavaClassBuilder(structureObjectClassName, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = generatorState.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		generatorState.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
		         structureObjectClassName,
		         null,
		         GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
		         null);

		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);

		generateStructureObjectConstructor(input, generatorState, cw,
		                                   structureClassClassName, structureClassClassDesc, structureObjectClassName);
		generateStructureObjectInitSlotsMap(input, generatorState, cw,
		                                    structureObjectClassName);

		cw.visitEnd();

		classBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the constructor for the generated {@link StructureObjectStruct} being written to
	 * via the provided {@link ClassWriter}. The generation will perform the following operations:
	 * <ol>
	 * <li>Generating the code to grab the static singleton {@code INSTANCE} field from the associated {@link
	 * StructureClassStruct}</li>
	 * <li>Generating the code to retrieve the {@link SymbolStructImpl} identifying the {@link StructureObjectStruct}</li>
	 * <li>If the provided {@link DefstructStruct#includeStructureClass} is {@code null}, generating {@code null} to be
	 * used as the parent {@link StructureObjectStruct} instance</li>
	 * <li>If the provided {@link DefstructStruct#includeStructureClass} is not {@code null}, generating the code to
	 * grab the static singleton {@code INSTANCE} field from the associated {@link StructureClassStruct} of the parent
	 * {@link StructureObjectStruct} and invoking {@link StructureClassStruct#newInstance()} to retrieve and use a new
	 * instance of the associated parent {@link StructureObjectStruct}</li>
	 * <li>Optionally generating the code to invoke the initialization of the {@link StructureObjectStruct#slots} map
	 * depending on whether or not the {@link DefstructStruct#slots} list is empty or not</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered (no included parent, with slots):
	 * <pre>
	 * {@code
	 * public FOOStructureObject_1() {
	 *      FOOStructureClass_1 var10001 = FOOStructureClass_1.INSTANCE;
	 *
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("FOO").getSymbol();
	 *
	 *      super(var10001, var2, (StructureObjectStruct)null);
	 *      this.initSlotsMap();
	 * }
	 * }
	 * </pre>
	 * The following is the example Java code generated when {@code (compiler:%defstruct bar foo make-bar nil)} is
	 * encountered (included parent, without slots):
	 * <pre>
	 * {@code
	 * public BARStructureObject_1() {
	 *      BARStructureClass_1 var10001 = BARStructureClass_1.INSTANCE;
	 *
	 *      PackageStruct var1 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var2 = var1.intern("BAR").getSymbol();
	 *
	 *      super(var10001, var2, FOOStructureClass_1.INSTANCE.newInstance());
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link DefstructStruct} containing the {@link StructureObjectStruct} metadata needed to generate the
	 * 		constructor code
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the constructor code for
	 * @param structureClassClassName
	 * 		the {@link String} containing the name of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureClassClassDesc
	 * 		the {@link String} containing the type descriptor of the {@link StructureClassStruct} for this {@link
	 * 		StructureObjectStruct}
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the constructor code
	 * 		for
	 */
	private static void generateStructureObjectConstructor(final DefstructStruct input, final GeneratorState generatorState,
	                                                       final ClassWriter cw,
	                                                       final String structureClassClassName,
	                                                       final String structureClassClassDesc,
	                                                       final String structureObjectClassName) {
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC,
		                                        GenerationConstants.INIT_METHOD_NAME,
		                                        GenerationConstants.INIT_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  structureClassClassName,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  structureClassClassDesc);

		final SymbolStructImpl structureSymbol = input.getStructureSymbol();
		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(structureSymbol, generatorState, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		final StructureClassStruct includeStructureClass = input.getIncludeStructureClass();
		if (includeStructureClass == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			final String includeStructureClassClassName = Type.getInternalName(includeStructureClass.getClass());
			final String includeStructureClassClassDesc = 'L' + includeStructureClassClassName + ';';
			mv.visitFieldInsn(Opcodes.GETSTATIC,
			                  includeStructureClassClassName,
			                  GenerationConstants.SINGLETON_INSTANCE,
			                  includeStructureClassClassDesc);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   includeStructureClassClassName,
			                   STRUCTURE_CLASS_NEW_INSTANCE_METHOD_NAME,
			                   STRUCTURE_CLASS_NEW_INSTANCE_METHOD_DESC,
			                   false);
		}
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.STRUCTURE_OBJECT_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRUCTURE_OBJECT_INIT_SCS_SS_SOS_METHOD_DESC,
		                   false);

		final List<SymbolStructImpl> slots = input.getSlots();
		if (!slots.isEmpty()) {
			// No need to call this method, as there are no slots to initialize
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
			                   structureObjectClassName,
			                   INIT_SLOTS_MAP_METHOD_NAME,
			                   INIT_SLOTS_MAP_METHOD_DESC,
			                   false);
		}

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}

	/**
	 * Private method for generating the {@code initSlotsMap} initializing method for the generated {@link
	 * StructureObjectStruct} being written to via the provided {@link ClassWriter}. The generation will perform the
	 * following operations:
	 * <ol>
	 * <li>Returning early and avoid generating the method unnecessarily if the {@link List} of {@link
	 * DefstructStruct#slots} is empty</li>
	 * <li>Generating the code to retrieve the {@link StructureObjectStruct#slots} field</li>
	 * <li>Generating the code to retrieve the slot {@link SymbolStructImpl} and store a {@code null} value associated with
	 * that symbol into the {@link StructureObjectStruct#slots} {@link Map}</li>
	 * </ol>
	 * The following is the example Java code generated when {@code (compiler:%defstruct foo nil make-foo nil a)} is
	 * encountered:
	 * <pre>
	 * {@code
	 * protected void initSlotsMap() {
	 *      Map var1 = this.slots;
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.intern("A").getSymbol();
	 *      var1.put(var3, (Object)null);
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link DefstructStruct} containing the slots {@link SymbolStructImpl}s to initialize
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param cw
	 * 		the current {@link ClassWriter} to generate the method code for
	 * @param structureObjectClassName
	 * 		the {@link String} containing the name of the {@link StructureObjectStruct} to generate the {@code
	 * 		initSlotsMap} method code for
	 */
	private static void generateStructureObjectInitSlotsMap(final DefstructStruct input, final GeneratorState generatorState,
	                                                        final ClassWriter cw,
	                                                        final String structureObjectClassName) {
		final List<SymbolStructImpl> slots = input.getSlots();
		if (slots.isEmpty()) {
			// No need to generate this method, as there are no slots to initialize
			return;
		}

		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
		                                        INIT_SLOTS_MAP_METHOD_NAME,
		                                        INIT_SLOTS_MAP_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, thisStore);
		mv.visitFieldInsn(Opcodes.GETFIELD, structureObjectClassName, SLOTS_FIELD, GenerationConstants.JAVA_MAP_DESC);
		final int slotsFieldStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, slotsFieldStore);

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int slotStore = methodBuilder.getNextAvailableStore();

		for (final SymbolStructImpl slot : slots) {
			CodeGenerators.generateSymbol(slot, generatorState, packageStore, slotStore);

			mv.visitVarInsn(Opcodes.ALOAD, slotsFieldStore);
			mv.visitVarInsn(Opcodes.ALOAD, slotStore);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_MAP_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitInsn(Opcodes.RETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();
	}
}
