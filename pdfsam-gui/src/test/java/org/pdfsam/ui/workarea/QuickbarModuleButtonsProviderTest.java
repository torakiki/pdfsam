/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ago/2014
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
package org.pdfsam.ui.workarea;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.tool.Tool;
import org.pdfsam.test.AdditionalDefaultPriorityTestTool;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.HighPriorityTestTool;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.test.LowPriorityTestTool;

/**
 * @author Andrea Vacondio
 */
public class QuickbarModuleButtonsProviderTest {
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();

    private LowPriorityTestTool lowPrio = new LowPriorityTestTool();
    private DefaultPriorityTestTool defaultPrio = new DefaultPriorityTestTool();
    private AdditionalDefaultPriorityTestTool additionalDefaultPrio = new AdditionalDefaultPriorityTestTool();
    private HighPriorityTestTool highPrio = new HighPriorityTestTool();
    private List<Tool> tools = Arrays.asList(defaultPrio, highPrio, lowPrio, additionalDefaultPrio);
    private QuickbarModuleButtonsProvider victim;

    @Before
    public void setUp() {
        victim = new QuickbarModuleButtonsProvider(tools);
    }

    @Test
    public void prioritizedUsedOnTop() {
        assertEquals(Arrays.asList(highPrio.id(), additionalDefaultPrio.id(), defaultPrio.id(), lowPrio.id()),
                     victim.buttons().stream().map(ModuleButton::moduleId).collect(Collectors.toList()));
    }

}
