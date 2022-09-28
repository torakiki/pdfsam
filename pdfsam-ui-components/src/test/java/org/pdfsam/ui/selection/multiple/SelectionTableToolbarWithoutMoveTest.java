/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14 dic 2015
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
package org.pdfsam.ui.selection.multiple;

import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.selection.multiple.SelectionTableToolbar.AddButton;
import org.pdfsam.ui.selection.multiple.SelectionTableToolbar.MoveDownButton;
import org.pdfsam.ui.selection.multiple.SelectionTableToolbar.MoveUpButton;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SelectionTableToolbarWithoutMoveTest extends ApplicationTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    private SelectionTableToolbar victim;

    @Override
    public void start(Stage stage) {
        victim = new SelectionTableToolbar(MODULE, false);
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void moveDownIsMissing() {
        assertTrue(lookup(b -> b instanceof AddButton).tryQuery().isPresent());
        assertTrue(lookup(b -> b instanceof MoveUpButton).tryQuery().isEmpty());
    }

    @Test
    public void moveUpIsMissing() {
        assertTrue(lookup(b -> b instanceof AddButton).tryQuery().isPresent());
        assertTrue(lookup(b -> b instanceof MoveDownButton).tryQuery().isEmpty());
    }
}
