/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14 dic 2015
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
package org.pdfsam.ui.selection.multiple;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.exceptions.NoNodesFoundException;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.selection.multiple.SelectionTableToolbar.MoveDownButton;
import org.pdfsam.ui.selection.multiple.SelectionTableToolbar.MoveUpButton;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SelectionTableToolbarWithoutMoveTest extends GuiTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);

    @Override
    protected Parent getRootNode() {
        SelectionTableToolbar victim = new SelectionTableToolbar(MODULE, false);
        victim.setId("victim");
        return victim;
    }

    @Test(expected = NoNodesFoundException.class)
    public void moveDownIsMissing() {
        find(b -> b instanceof MoveUpButton);
    }

    @Test(expected = NoNodesFoundException.class)
    public void moveUpIsMissing() {
        find(b -> b instanceof MoveDownButton);
    }
}
