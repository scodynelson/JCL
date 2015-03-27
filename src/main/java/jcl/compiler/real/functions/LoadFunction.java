/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

public class LoadFunction {

//        extends FunctionBaseClass implements Function5 {
//
//    public static final Load FUNCTION = new Load();
//    public static final Symbol SYMBOL = Package.CommonLisp.intern("LOAD")[0];
//
//    static { SYMBOL.setFunction(FUNCTION); }
//
//    /** Creates a new instance of Load */
//    private LoadFunction() {
//    }
//
//    /**
//     * The apply method passes the first object into the array to the funcall.
//     * @param argsList An array of objects.
//     * @return the funcall of the first object, which is the name of the file.
//     */
//    public Object apply(lisp.common.type.List argsList) {
//        Object[] args = argsList.toArray();
//        Object[] holder = new Object[5];
//        holder[0] = null;       // filespec
//        holder[1] = Boolean.T;  // verbose
//        holder[2] = Boolean.NIL;// print
//        holder[3] = Boolean.T;  // if-not-exists
//        holder[4] = Keyword.Default;  // external-format
//
//        if (args.length == 0) {
//            throw new WrongNumberOfArgsException("too few arguments given to LOAD: ");
//        } else if (args.length > 5) {
//            throw new WrongNumberOfArgsException("too many arguments given to LOAD: " + args.length);
//        } else {
//            for (int index = 0; index < args.length; index++) {
//                holder[index] = args[index];
//            }
//            return funcall(holder[0], holder[1], holder[2], holder[3], holder[4]);
//        }
//    }
//
//    /**
//     * The funcall method loads a filename, allowing for an open, read and eval to take place.
//     * @param fileSpec Object ???
//     * @param loadVerbose Object ???
//     * @param loadPrint Object ???
//     * @param ifNotExists Object ???
//     * @param extFormat Object ???
//     * @return The lisp.common.function.Values.FUNCTION.apply of the results.
//     * @see lisp.system.function.Open
//     * @see lisp.common.function.Read
//     * @see lisp.common.function.Eval
//     */
//    @SuppressWarnings("unchecked")
//    public Object funcall(Object fileSpec, Object loadVerbose, Object loadPrint, Object ifNotExists, Object extFormat) {
//        try {
//            ((lisp.system.SymbolImpl) Variable.Readtable).bind(Variable.Readtable.getValue());
//            ((lisp.system.SymbolImpl) Variable.Package).bind(Variable.Package.getValue());
//            if (fileSpec instanceof Symbol) {
//                return loadStartupCode((Symbol) fileSpec, loadVerbose, loadPrint, ifNotExists, extFormat);
//            } else {
//                File loadFile;
//                if (fileSpec instanceof Pathname) {
//                    loadFile = new File(Namestring.FUNCTION.funcall(fileSpec).toString());
//                } else if (fileSpec instanceof String) {
//                    loadFile = new File(fileSpec.toString());
//                } else if (fileSpec instanceof String) {
//                    loadFile = new File((String) fileSpec);
//                } else { // Wrong Type
//                    throw new TypeErrorException("LOAD: Expected a Pathname or a String, got " + fileSpec.getClass());
//                }
//
//                if (isCompiledCode(loadFile)) {
//                    return loadCompiledCode(loadFile, loadVerbose, loadPrint, ifNotExists, extFormat);
//                } else {
//                    return loadSourceCode(loadFile, loadVerbose, loadPrint, ifNotExists, extFormat);
//                }
//            }
//        } finally {
//            ((lisp.system.SymbolImpl) Variable.Readtable).unbind();
//            ((lisp.system.SymbolImpl) Variable.Package).unbind();
//        }
//    }
//
//    /** Routine to load compiled code */
//    private boolean isCompiledCode(File fileSpec) {
//        return (fileSpec.toString().endsWith(".lar")) || (fileSpec.toString().endsWith(".jar"));
//    }
//
//    private Object loadSourceCode(File loadFile, Object loadVerbose, Object loadPrint, Object ifNotExists, Object extFormat) {
//        try {
//            // Object file = null;
//            // Object[] holder = new Object[1];
//            // holder[0] = loadFile;
//            lisp.common.type.List holderList = lisp.common.type.List.Factory.newInstance(loadFile.toString());
//            Object file = lisp.system.function.Open.FUNCTION.apply(holderList);
//
//            Object eofValue = new Object();
//            Object readValue = null;
//
//            if (loadVerbose != Boolean.NIL) {
//                System.out.println("; Loading file " + loadFile);
//            }
//            while ((readValue = Read.FUNCTION.funcall(file, Boolean.NIL, eofValue)) != eofValue) {
//                try {
////		    ((Print)Print.FUNCTION).funcall("Read: " + readValue);
//                    Object item = Eval.FUNCTION.funcall(readValue);
//                    if (loadPrint != Boolean.NIL) {
//                        System.out.println(item);
//                    }
//                } catch (lisp.common.exceptions.FunctionException e) {
//                    System.out.println("Exception in reading: " + e);
//                }
//            }
//            return Boolean.T;
//        } catch (FunctionException e) {
//            if (ifNotExists == Boolean.NIL) {
//                return Boolean.NIL;
//            } else {
//                throw new FunctionException("file " + loadFile + " does not exist");
//            }
//        }
//    }
//
//
//    // Notice that this no longer loads over a network. That should be restored after we prove that
//    // the new class loader works correctly. Then subclass the URIClassLoader
//    @SuppressWarnings("unchecked")
//    private Object loadCompiledCode(File loadFile, Object loadVerbose, Object loadPrint, Object ifNotExists, Object extFormat) {
//        try {
//            System.out.println("loadFile: " + loadFile);
//            JarFile jf = new JarFile(loadFile);
//            lisp.system.compiler.LispClassLoader loader = new lisp.system.compiler.LispClassLoader(jf);
//
//            Manifest m = jf.getManifest();
//            Attributes att = m.getMainAttributes();
//
//            String main = att.get(Attributes.Name.MAIN_CLASS).toString();
//
//            Class lambda2 = Class.forName(main, true, loader);
//            Constructor construct = lambda2.getConstructor();
//            Object ret = ((Function0)construct.newInstance()).funcall();
//
//            return ret;
//
//        } catch (ClassNotFoundException e) {
//            System.out.println("ClassNotFoundException");
//            e.printStackTrace();
//        } catch (InstantiationException e) {
//            System.out.println("InstantiationException");
//            e.printStackTrace();
//        } catch (IllegalAccessException e) {
//            System.out.println("IllegalAccessException");
//            e.printStackTrace();
//        } catch (java.lang.reflect.InvocationTargetException e) {
//            System.out.println("InvocationTargetException");
//            e.printStackTrace();
//        } catch (NoSuchMethodException e) {
//            System.out.println("NoSuchMethodException");
//            e.printStackTrace();
//        } catch (IOException e) {
//            System.out.println("IOException");
//            e.printStackTrace();
//        } catch (Exception e) {
//            System.out.println("Exception");
//            e.printStackTrace();
//        }
//        return null;
//    }
//
//    @SuppressWarnings("unchecked")
//    private Object loadStartupCode(Symbol loadFile, Object loadVerbose, Object loadPrint, Object ifNotExists, Object extFormat) {
//        try {
//            // turn the Symbol into a string
//            String main = loadFile.toString();
//
//            // get the current class loader
//            ClassLoader loader = lisp.common.CLforJava.CURRENT_CLASSLOADER;
//            // load the main lambda in the jar file
//            loader.loadClass(main);
//            Class lambda2 = Class.forName(main, true, loader);
//            Constructor construct = lambda2.getConstructor();
//            // run the wrap-around lambda expression
//            return ((Function0) construct.newInstance()).funcall();
//
//        } catch (ClassNotFoundException e) {
//            System.out.println("WARNING: class " + loadFile + " not found during loading of initialization JAR file.");
//        } catch (InstantiationException e) {
//            e.printStackTrace();
//        } catch (IllegalAccessException e) {
//            e.printStackTrace();
//        } catch (java.lang.reflect.InvocationTargetException e) {
//            e.printStackTrace();
//        } catch (NoSuchMethodException e) {
//            e.printStackTrace();
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//        return null;
//    }
            /*
            InputStream xmlStream = loader.getResourceAsStream(main + ".xml");

            if (xmlStream != null){
            DocumentBuilderFactory docBuildFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder parser = docBuildFactory.newDocumentBuilder();
            Document xmlDoc = parser.parse(xmlStream);

            NodeList docInstances = xmlDoc.getElementsByTagName("docInstance");

            //System.out.println("DocumentationDom: " + lisp.common.type.Variable.DocumentationDom.getValue());
            List<Object> mainList = lisp.common.type.Variable.DocumentationDom.getValue();

            //		if(mainList == Null.NIL){
            //		mainList = lisp.common.type.List.Factory.newInstance();
            //		} doesn't do anything - just returns NIL anyway

            for (int i = 0; i < docInstances.getLength(); i++){
            Node instance = docInstances.item(i);
            NamedNodeMap attributes = instance.getAttributes();

            if (attributes.getLength() > 0) {
            java.lang.String uidValue = instance.getAttributes().getNamedItem("docUID").getNodeValue();
            Symbol uid = Package.Compiler.intern(lisp.common.type.String.Factory.newInstance(uidValue))[0];
            mainList = mainList.push(instance).push(uid);
            }
            }
            lisp.common.type.Variable.DocumentationDom.setValue(mainList);
            }
             */
}
