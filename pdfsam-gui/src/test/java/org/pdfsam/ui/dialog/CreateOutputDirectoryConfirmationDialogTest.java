/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04 dic 2015
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
package org.pdfsam.ui.dialog;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.test.ClearEventStudioRule;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class CreateOutputDirectoryConfirmationDialogTest extends ApplicationTest {
    private boolean confirm = false;

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();
    private Button button;

    @BeforeClass
    public static void setUp() {
        ((I18nContext) I18nContext.getInstance()).refresh(
                new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        StylesConfig styles = mock(StylesConfig.class);
        CreateOutputDirectoryConfirmationDialog victim = new CreateOutputDirectoryConfirmationDialog(styles);
        button = new Button("show");
        button.setOnAction(a -> confirm = victim.response());
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShown() {
        clickOn(button);
        assertTrue(lookup(I18nContext.getInstance().i18n("The selected output directory does not exist"))
                .tryQuery().isPresent());
        assertTrue(lookup(I18nContext.getInstance().i18n("Do you want to create it?")).tryQuery().isPresent());
        clickOn(I18nContext.getInstance().i18n("No"));
    }

    @Test
    @Category(NoHeadless.class)
    public void cancel() {
        this.confirm = true;
        clickOn(button);
        clickOn(I18nContext.getInstance().i18n("No"));
        assertFalse(this.confirm);
    }

    @Test
    @Category(NoHeadless.class)
    public void overwrite() {
        this.confirm = false;
        clickOn(button);
        clickOn(I18nContext.getInstance().i18n("Yes"));
        assertTrue(this.confirm);
    }

}
