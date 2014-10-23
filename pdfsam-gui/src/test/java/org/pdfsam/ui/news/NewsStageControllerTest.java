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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.time.Instant;
import java.util.Collections;

import javafx.scene.Parent;
import javafx.scene.control.Button;

import javax.inject.Inject;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.NewsPolicy;
import org.pdfsam.ui.StageService;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.dashboard.preference.PreferenceComboBox;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class NewsStageControllerTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(NewsStageController.NEWSSTAGE_EVENTSTATION);

    @Inject
    private ApplicationContext applicationContext;
    @Inject
    private UserContext userContext;

    @Configuration
    @Lazy
    static class Config {
        @Bean
        public NewsStageController controller() {
            return new NewsStageController(service(), userContext());
        }

        @Bean
        public StageService service() {
            StageService service = mock(StageService.class);
            when(service.getLatestNewsStageDisplayInstant()).thenReturn(Instant.now());
            return service;
        }

        @Bean
        public UserContext userContext() {
            return mock(UserContext.class);
        }

        @Bean
        public PreferenceComboBox<KeyStringValueItem<String>> combo() {
            return new PreferenceComboBox<>(StringUserPreference.NEWS_POLICY, userContext());

        }

        @Bean
        public NewsStage stage() {
            return spy(new NewsStage(Collections.emptyList(), mock(StylesConfig.class), combo()));
        }

    }

    @Override
    protected Parent getRootNode() {
        Button button = new Button("show");
        button.setOnAction(e -> eventStudio().broadcast(new ShowStageRequest(),
                NewsStageController.NEWSSTAGE_EVENTSTATION));
        applicationContext.getBean(NewsStage.class);
        applicationContext.getBean(NewsStageController.class);
        return button;
    }

    @Test
    @DirtiesContext
    public void show() throws Exception {
        when(userContext.getNewsPolicy()).thenReturn(NewsPolicy.ALWAYS.toString());
        click("show");
        NewsStage stage = applicationContext.getBean(NewsStage.class);
        verify(stage).loadAndShow(any());
        FXTestUtils.invokeAndWait(() -> {
            stage.hide();
        }, 2);
    }

    @Test
    public void dontShow() throws Exception {
        when(userContext.getNewsPolicy()).thenReturn(NewsPolicy.NEVER.toString());
        click("show");
        NewsStage stage = applicationContext.getBean(NewsStage.class);
        verify(stage, never()).loadAndShow(any());
    }

}
