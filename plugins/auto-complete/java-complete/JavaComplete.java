import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class JavaComplete {
    public static void main(String[] args) {
        List<String> imports = new ArrayList<String>();
        List<String> classes = new ArrayList<String>();

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (arg.equals("-i") && i + 1 < args.length) {
                for (String imp : args[i + 1].split(":")) {
                    imports.add(imp.trim());
                }
                i++;
            } else {
                classes.add(arg.trim());
            }
        }
        
        try {
            System.out.println("(");
            for (String name : classes) {
                Class<?> clazz = lookupClass(imports, name);
                if (clazz != null) {
                    printClass(name, clazz, System.out);
                }
            }
            System.out.println(")");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Class<?> lookupClass(List<String> imports, String name) {
        // Generics is not supported yet
        name = name.replaceAll("<.*$", "");

        try {
            return Class.forName(name);
        } catch (ClassNotFoundException e) {}
        
        try {
            return Class.forName("java.lang." + name);
        } catch (ClassNotFoundException e) {}

        for (String imp : imports) {
            try {
                if (imp.endsWith(name)) {
                    return Class.forName(imp);
                } else if (imp.endsWith("*")) {
                    return Class.forName(imp.substring(0, imp.length() - 1) + name);
                }
            } catch (ClassNotFoundException e) {}
        }

        return null;
    }

    private static void printClass(String name, Class<?> clazz, PrintStream out) {
        StringBuilder sb = new StringBuilder();

        // name
        sb.append("(\"").append(name).append("\" ");

        // methods
        sb.append("(");
        for (Method method : clazz.getMethods()) {
            sb.append("\"").append(method.getName()).append("\" ");
        }
        sb.append(") ");

        // fields
        sb.append("(");
        for (Field field : clazz.getFields()) {
            sb.append("\"").append(field.getName()).append("\" ");
        }
        sb.append(") ");

        sb.append(")");

        out.println(sb.toString());
    }
}

