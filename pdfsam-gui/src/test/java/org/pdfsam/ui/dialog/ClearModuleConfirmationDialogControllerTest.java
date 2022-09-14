/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25 ott 2020
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class ClearModuleConfirmationDialogControllerTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();
    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    private Button button;
    private HitTestListener<ClearModuleEvent> listener;
    private UserContext context;

    @BeforeClass
    public static void setUp() {
        ((I18nContext) I18nContext.getInstance()).refresh(
                new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        StylesConfig styles = mock(StylesConfig.class);
        context = mock(UserContext.class);
        when(context.isAskClearConfirmation()).thenReturn(Boolean.TRUE);
        ClearModuleConfirmationDialog victim = new ClearModuleConfirmationDialog(styles);
        button = new Button("show");
        new ClearModuleConfirmationDialogController(() -> new ClearModuleConfirmationDialog(styles), context);
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
        listener = new HitTestListener<ClearModuleEvent>();
    }

    @Test
    public void negativeTest() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearModuleEvent("module", true, true)));
        eventStudio().add(ClearModuleEvent.class, listener, "module");
        clickOn("show");
        clickOn(I18nContext.getInstance().i18n("No"));
        assertFalse(listener.isHit());
    }

    @Test
    public void noAskConfirmation() {
        when(context.isAskClearConfirmation()).thenReturn(Boolean.TRUE);
        button.setOnAction(a -> eventStudio().broadcast(new ClearModuleEvent("module", true, false)));
        eventStudio().add(ClearModuleEvent.class, listener, "module");
        clickOn("show");
        assertTrue(listener.isHit());
    }

    @Test
    public void noAskConfirmationPreference() {
        when(context.isAskClearConfirmation()).thenReturn(Boolean.FALSE);
        button.setOnAction(a -> eventStudio().broadcast(new ClearModuleEvent("module", true, true)));
        eventStudio().add(ClearModuleEvent.class, listener, "module");
        clickOn("show");
        assertTrue(listener.isHit());
    }

    @Test
    @Category(NoHeadless.class)
    public void positiveTest() {
        when(context.isAskClearConfirmation()).thenReturn(Boolean.TRUE);
        button.setOnAction(a -> eventStudio().broadcast(new ClearModuleEvent("module", true, true)));
        eventStudio().add(ClearModuleEvent.class, listener, "module");
        clickOn("show");
        clickOn(I18nContext.getInstance().i18n("Yes"));
        assertTrue(listener.isHit());
    }
}
