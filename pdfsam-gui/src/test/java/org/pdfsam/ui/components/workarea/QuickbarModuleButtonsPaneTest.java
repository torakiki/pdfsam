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
package org.pdfsam.ui.components.workarea;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.test.LowPriorityTestTool;
import org.pdfsam.model.ui.SetActiveToolRequest;

/**
 * @author Andrea Vacondio
 *
 */
public class QuickbarModuleButtonsPaneTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private QuickbarModuleButtonsPane victim;
    private List<ModuleButton> buttons;

    @Before
    public void setUp() {
        buttons = new ArrayList<>();
        buttons.add(new ModuleButton(new DefaultPriorityTestTool()));
        buttons.add(new ModuleButton(new LowPriorityTestTool()));
        QuickbarModuleButtonsProvider provider = mock(QuickbarModuleButtonsProvider.class);
        when(provider.buttons()).thenReturn(buttons);
        victim = new QuickbarModuleButtonsPane(provider);
    }

    @Test
    public void onSetCurrentModuleRequest() {
        buttons.forEach(m -> assertFalse(m.isSelected()));
        SetActiveToolRequest r = SetActiveToolRequest.activeteModule(DefaultPriorityTestTool.ID);
        victim.onSetCurrentModuleRequest(r);
        assertTrue(buttons.stream().filter(ModuleButton::isSelected)
                .allMatch(m -> m.moduleId().equals(DefaultPriorityTestTool.ID)));
        assertTrue(buttons.stream().filter(m -> !m.moduleId().equals(DefaultPriorityTestTool.ID))
                .noneMatch(ModuleButton::isSelected));
    }

    @Test
    public void displayTest() {
        buttons.forEach(m -> assertFalse(m.isDisplayText()));
        victim.setDisplayText(true);
        buttons.forEach(m -> assertTrue(m.isDisplayText()));
    }
}
