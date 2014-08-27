/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ago/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.test.SecondTestModule;
import org.pdfsam.test.TestModule;
import org.pdfsam.ui.event.SetActiveModuleRequest;

/**
 * @author Andrea Vacondio
 *
 */
public class QuickbarModuleButtonsTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private QuickbarModuleButtons victim;
    private Set<ModuleButton> buttons;

    @Before
    public void setUp() {
        buttons = new HashSet<>();
        buttons.add(new ModuleButton(new TestModule()));
        buttons.add(new ModuleButton(new SecondTestModule()));
        QuickbarModuleButtonsProvider provider = mock(QuickbarModuleButtonsProvider.class);
        when(provider.buttons()).thenReturn(buttons);
        victim = new QuickbarModuleButtons(provider);
    }

    @Test
    public void onSetCurrentModuleRequest() {
        buttons.forEach(m -> assertFalse(m.isSelected()));
        SetActiveModuleRequest r = SetActiveModuleRequest.activeteModule(TestModule.ID);
        victim.onSetCurrentModuleRequest(r);
        assertTrue(buttons.stream().filter(ModuleButton::isSelected).allMatch(m -> m.moduleId().equals(TestModule.ID)));
        assertTrue(buttons.stream().filter(m -> !m.moduleId().equals(TestModule.ID))
                .noneMatch(ModuleButton::isSelected));
    }

    @Test
    public void displayTest() {
        buttons.forEach(m -> assertFalse(m.isDisplayText()));
        victim.setDisplayText(true);
        buttons.forEach(m -> assertTrue(m.isDisplayText()));
    }
}
