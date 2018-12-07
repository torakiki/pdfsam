/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09 feb 2017
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
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
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
public class LenientExecutionConfirmationDialogTest extends ApplicationTest {
    private boolean confirm = false;

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        StylesConfig styles = mock(StylesConfig.class);
        LenientExecutionConfirmationDialog victim = new LenientExecutionConfirmationDialog(styles);
        Button button = new Button("show");
        button.setOnAction(a -> confirm = victim.response());
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void contentIsShown() {
        clickOn("show");
        assertTrue(lookup(DefaultI18nContext.getInstance().i18n("PDFsam can try to overcome the failure")).tryQuery()
                .isPresent());
        assertTrue(lookup(DefaultI18nContext.getInstance()
                .i18n("It may result in PDF files with partial or missing data, proceed anyway?")).tryQuery()
                        .isPresent());
        clickOn(DefaultI18nContext.getInstance().i18n("No"));
    }

    @Test
    @Category(NoHeadless.class)
    public void no() {
        this.confirm = true;
        clickOn("show");
        clickOn(DefaultI18nContext.getInstance().i18n("No"));
        assertFalse(this.confirm);
    }

    @Test
    public void yes() {
        this.confirm = false;
        clickOn("show");
        clickOn(DefaultI18nContext.getInstance().i18n("Yes"));
        assertTrue(this.confirm);
    }
}
