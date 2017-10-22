/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2011
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam;

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
    private Collection<? extends Object> packages = new ArrayList<>();

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
