/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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
package org.pdfsam.ui.components.dialog;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Locale;
import java.util.Optional;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.output.ExistingOutputPolicy;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class OverwriteConfirmationDialogTest extends ApplicationTest {

    private Optional<ExistingOutputPolicy> response = Optional.empty();

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    @BeforeClass
    public static void setUp() {
        ((I18nContext) I18nContext.getInstance()).refresh(
                new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        StylesConfig styles = mock(StylesConfig.class);
        OverwriteConfirmationDialog victim = new OverwriteConfirmationDialog(styles);
        Button button = new Button("show");
        button.setOnAction(a -> response = victim.title("Title").messageTitle("MessageTitle")
                .messageContent("MessageContent")
                .buttons(victim.defaultButton("Overwrite", ExistingOutputPolicy.OVERWRITE),
                        victim.button("Rename", ExistingOutputPolicy.RENAME),
                        victim.button("Skip", ExistingOutputPolicy.SKIP), victim.cancelButton("Cancel"))
                .response());
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShown() {
        clickOn("show");
        assertTrue(lookup("MessageTitle").tryQuery().isPresent());
        assertTrue(lookup("MessageContent").tryQuery().isPresent());
        clickOn(i18n().tr("Cancel"));
    }

    @Test
    @Category(NoHeadless.class)
    public void cancel() {
        response = Optional.empty();
        clickOn("show");
        clickOn(i18n().tr("Cancel"));
        assertTrue(response.isEmpty());
    }

    @Test
    @Category(NoHeadless.class)
    public void overwrite() {
        response = Optional.empty();
        clickOn("show");
        clickOn("Overwrite");
        assertFalse(response.isEmpty());
        assertEquals(ExistingOutputPolicy.OVERWRITE, response.get());
    }

    @Test
    @Category(NoHeadless.class)
    public void skip() {
        response = Optional.empty();
        clickOn("show");
        clickOn("Skip");
        assertFalse(response.isEmpty());
        assertEquals(ExistingOutputPolicy.SKIP, response.get());
    }

    @Test
    @Category(NoHeadless.class)
    public void rename() {
        response = Optional.empty();
        clickOn("show");
        clickOn("Rename");
        assertFalse(response.isEmpty());
        assertEquals(ExistingOutputPolicy.RENAME, response.get());
    }

    @Test
    public void esc() {
        response = Optional.empty();
        clickOn("show");
        push(KeyCode.ESCAPE);
        assertTrue(response.isEmpty());
    }

}
