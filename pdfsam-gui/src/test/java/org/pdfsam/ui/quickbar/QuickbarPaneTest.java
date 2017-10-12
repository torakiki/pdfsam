/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.ui.quickbar;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class QuickbarPaneTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Override
    protected Parent getRootNode() {
        BaseQuickbarButtonsPane buttons = new BaseQuickbarButtonsPane();
        buttons.setId("buttons");
        return new QuickbarPane(buttons);
    }

    @Test
    public void click() {
        BaseQuickbarButtonsPane buttons = find("#buttons");
        assertFalse(buttons.isDisplayText());
        click(".quickbar-expand-button");
        assertTrue(buttons.isDisplayText());
    }
}
