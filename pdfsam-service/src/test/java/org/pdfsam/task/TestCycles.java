package org.pdfsam.task;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import jdepend.framework.JDepend;
import jdepend.framework.JavaPackage;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Unit test to test against cycles.
 * 
 * @author Andrea Vacondio
 * @see http://www.softwarepoets.org/2009/04/unit-tests-to-check-against-cyclic.html
 */
public class TestCycles {

    private static final Logger LOG = LoggerFactory.getLogger(TestCycles.class);

    private JDepend jdepend = new JDepend();
    private Collection<? extends Object> packages = new ArrayList<Object>();

    @Before
    public void setUp() throws IOException {
        jdepend.addDirectory("target/classes");
        packages = jdepend.analyze();
    }

    @Test
    public void cycleTest() {
        for (Object p : packages) {
            JavaPackage pack1 = (JavaPackage) p;
            Assert.assertFalse(pack1.getName() + " failed.", pack1.containsCycle());
            LOG.debug(String.format("%s analyzed for cycles.", pack1.getName()));
        }
    }
}
