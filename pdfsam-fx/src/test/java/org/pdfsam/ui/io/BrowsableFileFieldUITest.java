/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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

import java.util.Arrays;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.support.io.FileType;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.testfx.api.FxAssert;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsableFileFieldUITest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Override
    public void start(Stage stage) {
        BrowsableFileField victimBlank = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victimBlank.enforceValidation(false, true);
        victimBlank.getStyleClass().add("victim-blank");

        BrowsableFileField victimNoBlank = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victimNoBlank.enforceValidation(false, false);
        victimNoBlank.getStyleClass().add("victim-no-blank");
        Scene scene = new Scene(new HBox(victimBlank, victimNoBlank));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validBlank() {
        BrowsableFileField victim = lookup(".victim-blank").queryAs(BrowsableFileField.class);
        clickOn(victim);
        type(KeyCode.TAB);
        FxAssert.verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.VALID);
    }

    @Test
    public void invalidBlank() {
        BrowsableFileField victim = lookup(".victim-no-blank").queryAs(BrowsableFileField.class);
        clickOn(victim);
        type(KeyCode.TAB);
        FxAssert.verifyThat(victim, v -> v.getTextField().getValidationState() == ValidationState.INVALID);
        Arrays.stream(Style.INVALID.css()).forEach(c -> assertTrue(lookup("." + c).tryQuery().isPresent()));
    }
}
