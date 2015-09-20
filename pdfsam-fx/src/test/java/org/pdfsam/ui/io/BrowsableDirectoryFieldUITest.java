/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/lug/2014
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
package org.pdfsam.ui.io;

import static org.loadui.testfx.Assertions.verifyThat;

import java.util.Arrays;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

import javafx.scene.Parent;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class BrowsableDirectoryFieldUITest extends GuiTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Override
    protected Parent getRootNode() {
        BrowsableDirectoryField victimNoBlank = new BrowsableDirectoryField(false);
        victimNoBlank.getStyleClass().add("victim-no-blank");
        BrowsableDirectoryField victimBlank = new BrowsableDirectoryField(true);
        victimBlank.getStyleClass().add("victim-blank");
        return new HBox(victimBlank, victimNoBlank);
    }

    @Test
    public void validBlank() {
        BrowsableDirectoryField victim = find(".victim-blank");
        click(victim);
        type(KeyCode.TAB);
        verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.VALID);
    }

    @Test
    public void invalidBlank() {
        BrowsableDirectoryField victim = find(".victim-no-blank");
        click(victim);
        type(KeyCode.TAB);
        verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.INVALID);
        Arrays.stream(Style.INVALID.css()).forEach(c -> verifyThat(victim, v -> exists("." + c)));
    }

    @Test
    public void setGraphic() throws Exception {
        Label graphic = new Label("Chuck");
        BrowsableDirectoryField victim = find(".victim-no-blank");
        FXTestUtils.invokeAndWait(() -> victim.setGraphic(graphic), 1);
        verifyThat(victim, v -> exists("Chuck"));
    }

    @Test
    public void nullGraphicDoesntExplode() throws Exception {
        BrowsableDirectoryField victim = find(".victim-no-blank");
        FXTestUtils.invokeAndWait(() -> victim.setGraphic(null), 1);
    }

    @Test
    @Ignore
    public void dragAndDropExistingDirectory() {
        // Not sure how to test this
        /**
         * BrowsableDirectoryField victim = find(".victim-blank"); File inputFile = folder.newFolder(); dragTo(victim, inputFile); verifyThat(victim, v ->
         * inputFile.getAbsolutePath().equals(v.getTextField().getText()));
         */
    }

    @Test
    @Ignore
    public void dragAndDropExistingFile() {
        // Not sure how to test this
        /**
         * BrowsableDirectoryField victim = find(".victim-blank"); File inputFile = folder.newFile(); dragTo(victim, inputFile); verifyThat(victim, v ->
         * isBlank(v.getTextField().getText()));
         */
    }

    @Test
    public void dragAndDropNotExistingDirectory() {

    }

}
