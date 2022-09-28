/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/lug/2014
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
package org.pdfsam.ui.io;

import static org.junit.Assert.assertTrue;
import static org.pdfsam.core.support.validation.Validators.nonBlank;
import static org.pdfsam.core.support.validation.Validators.positiveInteger;
import static org.pdfsam.core.support.validation.Validators.validEmpty;

import java.util.Arrays;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.testfx.api.FxAssert;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsableDirectoryFieldUITest extends ApplicationTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Override
    public void start(Stage stage) {
        BrowsableDirectoryField victimNoBlank = new BrowsableDirectoryField();
        victimNoBlank.getTextField().setValidator(nonBlank());
        victimNoBlank.getStyleClass().add("victim-no-blank");
        BrowsableDirectoryField victimBlank = new BrowsableDirectoryField();
        victimBlank.getTextField().setValidator(validEmpty(positiveInteger()));
        victimBlank.getStyleClass().add("victim-blank");
        Scene scene = new Scene(new HBox(victimBlank, victimNoBlank));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validBlank() {
        BrowsableDirectoryField victim = lookup(".victim-blank").queryAs(BrowsableDirectoryField.class);
        clickOn(victim);
        type(KeyCode.TAB);
        FxAssert.verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.VALID);
    }

    @Test
    public void invalidBlank() {
        BrowsableDirectoryField victim = lookup(".victim-no-blank").queryAs(BrowsableDirectoryField.class);
        clickOn(victim);
        type(KeyCode.TAB);
        FxAssert.verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.INVALID);
        Arrays.stream(Style.INVALID.css()).forEach(c -> assertTrue(lookup("." + c).tryQuery().isPresent()));
    }

    @Test
    public void setGraphic() {
        Label graphic = new Label("Chuck");
        BrowsableDirectoryField victim = lookup(".victim-no-blank").queryAs(BrowsableDirectoryField.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.setGraphic(graphic));
        assertTrue(lookup("Chuck").tryQuery().isPresent());
    }

    @Test
    public void nullGraphicDoesntExplode() {
        BrowsableDirectoryField victim = lookup(".victim-no-blank").queryAs(BrowsableDirectoryField.class);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.setGraphic(null));
    }

    @Test
    public void copyPasteInQuotes() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            Clipboard clipboard = Clipboard.getSystemClipboard();
            ClipboardContent content = new ClipboardContent();
            content.putString("\"my path\"");
            clipboard.setContent(content);
        });

        BrowsableDirectoryField victim = lookup(".victim-blank").queryAs(BrowsableDirectoryField.class);
        clickOn(victim);
        press(KeyCode.CONTROL, KeyCode.V).release(KeyCode.V, KeyCode.CONTROL);
        FxAssert.verifyThat(victim, v -> v.getTextField().getText().equals("my path"));
    }

    @Test
    public void copyPasteNoQuotes() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            Clipboard clipboard = Clipboard.getSystemClipboard();
            ClipboardContent content = new ClipboardContent();
            content.putString("my path");
            clipboard.setContent(content);
        });

        BrowsableDirectoryField victim = lookup(".victim-blank").queryAs(BrowsableDirectoryField.class);
        clickOn(victim);
        press(KeyCode.CONTROL, KeyCode.V).release(KeyCode.V, KeyCode.CONTROL);
        FxAssert.verifyThat(victim, v -> v.getTextField().getText().equals("my path"));
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

}
