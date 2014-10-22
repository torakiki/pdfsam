/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2014
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
package org.pdfsam.ui.news;

import static com.google.code.tempusfugit.temporal.Duration.seconds;
import static com.google.code.tempusfugit.temporal.Timeout.timeout;
import static com.google.code.tempusfugit.temporal.WaitFor.waitOrTimeout;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.net.URISyntaxException;
import java.util.Collections;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

import javafx.scene.Parent;
import javafx.scene.control.Button;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.pdfsam.ui.dashboard.preference.PreferenceComboBox;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class NewsStageTest extends GuiTest {

    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    private Consumer<Boolean> onSuccess;
    private NewsStage newsStage;

    @Before
    public void setUp() {
        onSuccess = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        Button button = new Button("show");
        PreferenceComboBox<KeyStringValueItem<String>> newsDisplayPolicyCombo = new PreferenceComboBox<>(
                StringUserPreference.NEWS_POLICY, mock(UserContext.class));
        newsStage = new NewsStage(Collections.emptyList(), mock(StylesConfig.class), newsDisplayPolicyCombo);
        button.setOnAction(e -> newsStage.loadAndShow(onSuccess));
        return button;
    }

    @Test
    public void stageIsShown() throws URISyntaxException, InterruptedException, TimeoutException {
        newsStage.setNewsUrl(this.getClass().getClassLoader().getResource("htmltest.html").toURI().toString());
        click("show");
        waitOrTimeout(() -> newsStage.isShowing(), timeout(seconds(2)));
    }

    @Test
    public void onSuccessIsHit() throws URISyntaxException {
        newsStage.setNewsUrl(this.getClass().getClassLoader().getResource("htmltest.html").toURI().toString());
        click("show");
        verify(onSuccess, timeout(2000)).accept(false);
    }

    @Test
    public void onSuccessNeverIsHit() {
        newsStage.setNewsUrl("/this/does/not/exists");
        click("show");
        verify(onSuccess, after(1000).never()).accept(anyBoolean());
    }

    @Test
    public void wrapHrefToOpenNative() throws URISyntaxException, InterruptedException, TimeoutException {
        Listener<OpenUrlRequest> listener = mock(Listener.class);
        eventStudio().add(OpenUrlRequest.class, listener);
        newsStage.setNewsUrl(this.getClass().getClassLoader().getResource("htmltest.html").toURI().toString());
        click("show");
        waitOrTimeout(() -> newsStage.isShowing(), timeout(seconds(2)));
        click("#newsBrowser");
        verify(listener).onEvent(any());
    }

}
